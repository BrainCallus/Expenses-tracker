package model.service

import cats.Monad
import cats.data.EitherT
import cats.effect.IO
import cats.implicits.toFunctorOps
import cats.implicits.toFlatMapOps
import cats.effect.kernel.MonadCancelThrow
import cats.effect.unsafe.implicits.global
import doobie.{Read, Write}
import model.codecs.JsonReader
import model.codecs.JsonWriter._
import model.dao.algebr.{PayTypeProvider, ScheduledPayProvider}
import model.dao.algebr.PayTypeProvider.{FullExpenseOrPayEvidence, RawExpenseOrPayEvidence}
import model.dao.io.DbIOProvider.findNow
import model.dao.io.{ExpenseDao, ScheduledPayDao}
import model.entity.DatabaseEntity
import model.entity.pays.ScheduledPayStatus.FULFILLED
import model.entity.pays._
import model.exception.{DBException, FieldSpecifiedError}
import model.service.CommonService._
import model.util.DBUtils.DaoOptions
import model.util.DateUtil
import model.util.DateUtil.DateCalc
import model.validation.BaseValidatorsLib._
import model.validation.ValidationResult.MonadValidationResult._
import model.validation.ValidationResult._

import java.time.LocalDate

trait ExpenseServiceAlg[F[_]] {
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

object ExpenseServiceAlg {
  def make[F[_]: Monad](implicit
    commonService: CommonServiceAlg[F],
    userOptionService: UserOptionServiceAlg[F],
    payProvider: PayTypeProvider[F],
    scheduledPayProvider: ScheduledPayProvider[F],
    F: MonadCancelThrow[F]
  ): ExpenseServiceAlg[F] = new ExpenseServiceAlg[F] {
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
            MonadCancelThrow
              .apply[F]
              .redeem[Unit, Either[FieldSpecifiedError, Unit]](insertPay)(
                err => Left(DBException("general", "SQLException. Unable to save operation")),
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
                    err => Left[FieldSpecifiedError, Int](DBException("error", "SQLException")),
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
        r <- actionFromOption(opt)(
          EitherT.fromEither[F](Left[FieldSpecifiedError, Int](ValidationError("error", "Scheduled pay not found")))
        )(pay => {
          if (userId != pay.id) {
            EitherT.fromEither[F](Left[FieldSpecifiedError, Int](ValidationError("error", "Scheduled pay not found")))
          } else {
            val update = for {
              _ <- scheduledPayProvider.updatePayStatus(pay, newStatus)
              _ <- newStatus match {
                case FULFILLED => payProvider.insert(ExpenseRaw(pay.sum, pay.expenseType, pay.userId, pay.date))
                case _         => F.pure(())
              }
            } yield ()
            EitherT.apply(
              F.redeem(update)(
                err => Left[FieldSpecifiedError, Int](DBException("error", "SQLException")),
                _ => Right(1)
              )
            )
          }
        })
      } yield r
    }
  }

  def addExpenseOrSchedulePay[ExpenseOrPay <: PayType: Write: JsonReader](
    requestParams: Map[String, Seq[String]],
    isExpense: Boolean
  )(implicit ev: PayTypeProvider.RawExpenseOrPayEvidence[ExpenseOrPay]): Either[FieldSpecifiedError, Unit] = {
    withValidation[ExpenseOrPay, Either[FieldSpecifiedError, Unit]](requestParams, validatorsForAdd(isExpense)) {
      pay: ExpenseOrPay =>
        val y = for {
          _ <- setUpdateLastTimeUpdatedOption(pay.userId)
          _ <- ExpenseDao.insert(pay)
        } yield ()
        y.redeem(_ => Left(DBException("general", "SQLException")), r => Right(r)).unsafeRunSync()

    }.joinRight
  }

  def deleteExpenseOrScheduledPay[ExpenseOrPay <: DatabaseEntity with PayType: JsonReader: Read](
    id: Long,
    userId: Long,
    isExpense: Boolean
  )(implicit ev: FullExpenseOrPayEvidence[ExpenseOrPay]): Either[FieldSpecifiedError, Int] = {
    actionFromOption[ExpenseOrPay, Either[FieldSpecifiedError, Int]](
      ExpenseDao.find[ExpenseOrPay](id, if (isExpense) "expense" else "scheduledPay").safeRunToOption
    )(Right(1))(entity => {
      if (!(userId == entity.userId)) {
        Left(ValidationError("error", "You have no access"))
      } else {
        val i = for {
          _ <- ExpenseDao.delete[ExpenseOrPay](entity)
          r <- setUpdateLastTimeUpdatedOption(userId)
        } yield r
        i.toEitherDBException("error").map(_ => 1)
      }
    })
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

  def updateScheduledStatus(
    payId: Long,
    newStatus: ScheduledPayStatus,
    userId: Long
  ): Either[FieldSpecifiedError, Int] = {
    actionFromOption[ScheduledPayFull, Either[FieldSpecifiedError, Int]](
      ExpenseDao.find[ScheduledPayFull](payId, "false").safeRunToOption
    )(Left(ValidationError("error", "Scheduled pay not found")))(pay => {
      if (pay.userId == userId) {
        val i = for {
          _ <- ScheduledPayDao.updatePayStatus(pay, newStatus)
          _ <- newStatus match {
            case FULFILLED => ExpenseDao.insert(ExpenseRaw(pay.sum, pay.expenseType, pay.userId, pay.date))
            case _         => IO.unit
          }
          _ <- setUpdateLastTimeUpdatedOption(userId)
        } yield ()
        i.toEitherDBException().map(_ => 1)
      } else {
        Left(ValidationError("error", "Scheduled pay not found"))
      }
    })
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

  private def setUpdateLastTimeUpdatedOption(userId: Long): IO[Unit] =
    UserOptionService.setOption(userId, "lastTimeUpdated", findNow().toString)
  def getSumsByDay: List[ExpenseFull] => List[(LocalDate, Double)] =
    getSumsBy(_.date)(_).toList.sortWith((d1, d2) => d1._1.compareTo(d2._1) < 0)
  def getSumsByType: List[ExpenseFull] => Map[ExpensesType, Double] = getSumsBy(_.expenseType)(_)

  private def getSumsBy[P](groupper: ExpenseFull => P)(expenses: List[ExpenseFull]): Map[P, Double] = {
    expenses.groupMapReduce(groupper)(_.sum)(_ + _)
  }
}
