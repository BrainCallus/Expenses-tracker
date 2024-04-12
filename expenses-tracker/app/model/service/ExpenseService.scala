package model.service

import cats.data.EitherT
import cats.effect.kernel.MonadCancelThrow
import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Sync}
import cats.implicits.{toFlatMapOps, toFunctorOps}
import doobie.{Read, Write}
import model.codecs.JsonReader
import model.codecs.JsonWriter._
import model.dao.algebr.PayTypeProvider.{FullExpenseOrPayEvidence, RawExpenseOrPayEvidence}
import model.dao.algebr.{PayTypeProvider, ScheduledPayProvider}
import model.dao.io.DbIOProvider.findNow
import model.dao.io.ExpenseDao
import model.entity.DatabaseEntity
import model.entity.pays.ScheduledPayStatus.FULFILLED
import model.entity.pays._
import model.exception.{DBException, FieldSpecifiedError}
import model.util.DBUtils.DaoOptions
import model.util.DateUtil
import model.util.DateUtil.DateCalc
import model.validation.BaseValidatorsLib._
import model.validation.ValidationResult.MonadValidationResult._
import model.validation.ValidationResult._
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.time.LocalDate

trait ExpenseService[F[_]] {
  def addExpenseOrSchedulePay[ExpenseOrPay <: PayType: Write: JsonReader](
    requestParams: Map[String, Seq[String]],
    isExpense: Boolean
  )(implicit ev: PayTypeProvider.RawExpenseOrPayEvidence[ExpenseOrPay]): EitherT[F, FieldSpecifiedError, Unit]

  def deleteExpenseOrScheduledPay[ExpenseOrPay <: DatabaseEntity with PayType: JsonReader: Read](
    id: Long,
    userId: Long,
    isExpense: Boolean
  )(implicit ev: FullExpenseOrPayEvidence[ExpenseOrPay]): EitherT[F, FieldSpecifiedError, Int]
  def getPastTermScheduledPays(userId: Long): F[List[ScheduledPayFull]]
  def getPreviousPays(userId: Long): F[List[ScheduledPayFull]]

  def updateScheduledStatus(
    payId: Long,
    newStatus: ScheduledPayStatus,
    userId: Long
  ): EitherT[F, FieldSpecifiedError, Int]
}

object ExpenseService {
  def make[F[_]: MonadCancelThrow: Sync](implicit
    commonService: CommonService[F],
    userOptionService: UserOptionService[F],
    payProvider: PayTypeProvider[F],
    scheduledPayProvider: ScheduledPayProvider[F]
  ): ExpenseService[F] = new ExpenseService[F] {
    implicit val logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLogger
    override def addExpenseOrSchedulePay[ExpenseOrPay <: PayType: Write: JsonReader](
      requestParams: Map[String, Seq[String]],
      isExpense: Boolean
    )(implicit ev: RawExpenseOrPayEvidence[ExpenseOrPay]): EitherT[F, FieldSpecifiedError, Unit] = {
      commonService.withValidation(requestParams, validatorsForAdd(isExpense)) { pay: ExpenseOrPay =>
        {
          val insertPay = for {
            _ <- setUpdateLastTimeUpdatedOption_(pay.userId)
            _ <- payProvider.insert(pay)
          } yield ()
          EitherT.apply(
            MonadCancelThrow[F]
              .redeem[Unit, Either[FieldSpecifiedError, Unit]](insertPay)(
                err => {
                  logger.error(err)(s"Unexpected SQLException while inserting ${pay.getClass.getSimpleName}")
                  Left(DBException("general", "SQLException: Unable to save operation"))
                },
                Right(_)
              )
          )
        }
      }
    }

    private def setUpdateLastTimeUpdatedOption_(userId: Long): F[Unit] = {
      userOptionService.setOption(userId, "lastTimeUpdated", findNow().toString)
    }

    override def deleteExpenseOrScheduledPay[ExpenseOrPay <: DatabaseEntity with PayType: JsonReader: Read](
      id: Long,
      userId: Long,
      isExpense: Boolean
    )(implicit ev: FullExpenseOrPayEvidence[ExpenseOrPay]): EitherT[F, FieldSpecifiedError, Int] = {
      for {
        opt <- EitherT.right(payProvider.find[ExpenseOrPay](id, isExpense))
        v <- commonService.actionFromOption(opt)(EitherT.rightT[F, FieldSpecifiedError](1))(entity =>
          EitherT.apply({
            val MCancelThrow = MonadCancelThrow[F]
            if (userId != entity.userId) {
              MCancelThrow.pure(Left[FieldSpecifiedError, Int](ValidationError("error", "You have no access")))
            } else {
              val del = for {
                _ <- payProvider.delete(entity)
                _ <- setUpdateLastTimeUpdatedOption_(userId)
              } yield ()
              MCancelThrow.flatMap(MCancelThrow.attempt(del))(either =>
                MCancelThrow.pure(
                  either.fold(
                    err => {
                      logger.error(err)(s"Unexpected SQLException while deleting ${entity.getClass.getSimpleName}")
                      Left[FieldSpecifiedError, Int](
                        DBException("error", "SQLException: something went wrong while deleting operation")
                      )
                    },
                    _ => Right(1)
                  )
                )
              )
            }
          })
        )
      } yield v
    }

    override def getPastTermScheduledPays(userId: Long): F[List[ScheduledPayFull]] = {
      payProvider
        .findByPeriod[ScheduledPayFull](
          userId,
          LocalDate.of(1999, 1, 1),
          1.daysBefore(findNow().toLocalDate),
          isExpense = false
        )
        .map(pays => pays filter (_.status == ScheduledPayStatus.SCHEDULED))
    }

    override def getPreviousPays(userId: Long): F[List[ScheduledPayFull]] = {
      val start = LocalDate.of(1999, 1, 1)
      payProvider.findByPeriod[ScheduledPayFull](userId, start, 1.daysBefore(findNow().toLocalDate), isExpense = false)
    }

    override def updateScheduledStatus(
      payId: Long,
      newStatus: ScheduledPayStatus,
      userId: Long
    ): EitherT[F, FieldSpecifiedError, Int] = {
      for {
        opt <- EitherT.right(payProvider.find[ScheduledPayFull](payId, isExpense = false))
        r <- commonService.actionFromOption(opt)(
          EitherT.fromEither[F](Left[FieldSpecifiedError, Int](ValidationError("error", "Scheduled pay not found")))
        )(pay => {
          if (userId != pay.userId) {
            EitherT.fromEither[F](Left[FieldSpecifiedError, Int](ValidationError("error", "Scheduled pay not found")))
          } else {
            val update = for {
              _ <- scheduledPayProvider.updatePayStatus(pay, newStatus)
              _ <- newStatus match {
                case FULFILLED => payProvider.insert(ExpenseRaw(pay.sum, pay.expenseType, pay.userId, pay.date))
                case _         => MonadCancelThrow[F].pure(())
              }
            } yield ()
            EitherT.apply(
              MonadCancelThrow[F].redeem(update)(
                err => {
                  logger.error(err)(s"Unexpected SQLException. Cannot update pay status")
                  Left[FieldSpecifiedError, Int](
                    DBException("error", "SQLException: cannot change scheduled pay status")
                  )
                },
                _ => Right(1)
              )
            )
          }
        })
      } yield r
    }
  }

  def getPastTermScheduledPays(userId: Long): List[ScheduledPayFull] = {
    ExpenseDao
      .findForPeriod[ScheduledPayFull](
        userId,
        LocalDate.of(1999, 1, 1),
        1.daysBefore(findNow().toLocalDate),
        isExpense = false
      )
      .map(pays => pays filter (_.status == ScheduledPayStatus.SCHEDULED))
      .toEitherDBException("no exception here")
      .getOrElse(Nil)
  }

  def getPreviousPays(userId: Long): Either[DBException, List[ScheduledPayFull]] = {
    val start = LocalDate.of(1999, 1, 1)
    ExpenseDao
      .findForPeriod[ScheduledPayFull](userId, start, 1.daysBefore(findNow().toLocalDate), isExpense = false)
      .toEitherDBException()
  }

  def getSumsByDay: List[ExpenseFull] => List[(LocalDate, Double)] =
    getSumsBy(_.date)(_).toList.sortWith((d1, d2) => d1._1.compareTo(d2._1) < 0)

  def getSumsByType: List[ExpenseFull] => Map[ExpensesType, Double] = getSumsBy(_.expenseType)(_)

  private def getSumsBy[P](groupper: ExpenseFull => P)(expenses: List[ExpenseFull]): Map[P, Double] = {
    expenses.groupMapReduce(groupper)(_.sum)(_ + _)
  }

  private def validatorsForAdd(isExpense: Boolean) = List(
    ("sum", toOptionJsonValidator(doubleValidator(1, math.pow(10, 9)))),
    (
      "expenseType",
      toOptionJsonValidator((value: String) =>
        optionValidator[ExpensesType].mapRes(_.toString).run(ExpensesType.withNameInsensitiveOption(value))
      )
    ),
    ("userId", toOptionJsonValidator(longValidator(1, Int.MaxValue))),
    (
      "date",
      toOptionJsonValidator(
        NotBlankValidator()
          .>>!(RegexValidator("^\\d{4}-\\d{2}-\\d{2}$"))
          .>>!((value: String) => {
            flatMap(
              tailRecABC[String, Throwable, LocalDate](value)((v: String) =>
                pure(IO(LocalDate.parse(v)).attempt.unsafeRunSync())
              )
            )(v => {
              val today = DateUtil.getNow.toLocalDate.compareTo(v)

              if (isExpense && today < 0) {
                ValidationError("date", "You can select only previous dates")
              } else if (!isExpense && today >= 0) {
                ValidationError("date", "You can select only following dates")
              } else {
                Success(DateUtil.dateToDateTime(v))
              }
            })
          })
      )
    )
  )
}
