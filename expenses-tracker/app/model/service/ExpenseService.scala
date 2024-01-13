package model.service

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import doobie.{Read, Write}
import model.util.DateUtil.DateCalc
import model.codecs.JsonWriter._
import model.codecs.JsonReader
import model.validation.ValidationResult._
import model.validation.ValidationResult.MonadValidationResult._
import model.validation.BaseValidatorsLib._
import model.util.DBUtils.DaoOptions
import model.dao.algebr.PayTypeProvider
import model.dao.io.DbIOProvider.findNow
import model.dao.algebr.PayTypeProvider.FullExpenseOrPayEvidence
import model.dao.io.{ExpenseDao, ScheduledPayDao}
import model.entity.DatabaseEntity
import model.entity.pays.ScheduledPayStatus.FULFILLED
import model.entity.pays._
import model.exception.{DBException, FieldSpecifiedError}
import model.service.CommonService._
import model.util.DateUtil

import java.time.LocalDate
object ExpenseService {

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
      ).map(pays => pays filter(_.status == ScheduledPayStatus.SCHEDULED))
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
