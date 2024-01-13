package model.service

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import model.util.DateUtil.getNow
import model.util.DBUtils.DaoOptions
import model.dao.io.DbIOProvider.findNow
import model.dao.io.{DbIOProvider, ExpenseDao, UserOptionDao}
import model.entity.pays._
import model.entity.useroption._
import model.exception._
import model.py.HttpPyServSarima.{logger, runWithTimeOut}
import model.service.CommonService.actionFromOption
import model.service.ExpenseService.{getPreviousPays, getSumsByDay}
import model.validation.ValidationResult.ValidationError

import java.time.{LocalDate, LocalDateTime}

object ForecastService {
  def getMonthForecast(userId: Long): Either[FieldSpecifiedError, Double] = {
    requestFromPyOnNone(UserOptionDao.findByKeyAndUserId("forecast", userId), userId)((option: UserOptionDB) => {
      val today = getNow.toLocalDate
      val todayStart = LocalDateTime.of(today.getYear, today.getMonth, today.getDayOfMonth, 0, 0, 1)

      checkTermAndGetForecast(option, todayStart)(
        userId,
        (option: UserOptionDB) => {
          UserOptionDao.findByKeyAndUserId("lastTimeUpdated", userId) map { lastTimeUpdated =>
            checkTermAndGetForecast(option, lastTimeUpdated.updationTime)(
              userId,
              (option: UserOptionDB) => {
                requestFromPyOnNone(option.value.toDoubleOption, userId)(Right(_))
              }
            )
          } getOrElse Left(ValidationError("error", "User data not found"))
        }
      )
    })
  }

  private def checkTermAndGetForecast(
    forecastOpt: UserOptionDB,
    deadline: LocalDateTime
  )(userId: Long, action: UserOptionDB => Either[FieldSpecifiedError, Double]): Either[FieldSpecifiedError, Double] =
    if ((forecastOpt.updationTime compareTo deadline) >= 0) {
      action(forecastOpt)
    } else {
      requestPredictedFromPy(userId)
    }

  private def requestFromPyOnNone[T](option: Option[T], userId: Long)(
    action: T => Either[FieldSpecifiedError, Double]
  ): Either[FieldSpecifiedError, Double] = {
    actionFromOption(option)(requestPredictedFromPy(userId))(action(_))
  }

  private def requestPredictedFromPy(userId: Long): Either[FieldSpecifiedError, Double] = {
    (for {
      today <- IO.pure(DbIOProvider.findNow().toLocalDate)
      d <- ExpenseDao
        .findAllForUser[ExpenseFull](userId, issExpense = true)
        .map(data => (today.getMonth.length(today.isLeapYear) - today.getDayOfMonth, getSumsByDay(data)))
      predicted <- runWithTimeOut(d._1, d._2).map(either => {
        val cur = getExpensesFromCurrentMonth(d._2, today)
        val expectedSumFor = expectedSumForScheduled(userId)
        expectedSumFor.flatMap(ex => either flatMap (fs => Right(ex + fs.sum + cur)))
      })
      _ <- IO.pure(
        UserOptionService.setOption(userId, "forecast", predicted map (_.toString) getOrElse "invalid").unsafeRunSync()
      )
    } yield predicted)
      .redeemWith(
        e =>
          logger.info(e)("Unexpected error") flatMap (_ =>
            IO(Left(UnexpectedException("error", "Unexpected internal server error. Can't compute forecast")))
          ),
        IO.pure
      )
      .unsafeRunSync()
  }

  private def getExpensesFromCurrentMonth(expenses: List[(LocalDate, Double)], today: LocalDate): Double =
    (expenses filter (pair =>
      (pair._1 compareTo LocalDate.of(today.getYear, today.getMonthValue, 1)) >= 0
    ) map (_._2)).sum

  private def expectedSumForScheduled(userId: Long): Either[DBException, Double] = {
    val today = findNow().toLocalDate
    getPreviousPays(userId) map (x => computePercentMoneyComplete(x)) flatMap (avgComplete => {
      ExpenseDao findForPeriod [ScheduledPayFull] (userId, today, LocalDate.of(
        today.getYear,
        today.getMonth,
        today.getMonth.maxLength()
      ),
      isExpense = false) toEitherDBException "error" map (list => {
        (list filter (_.status == ScheduledPayStatus.SCHEDULED) map (_.sum)).sum * avgComplete
      })
    })
  }

  private def computePercentMoneyComplete(pays: List[ScheduledPayFull]): Double = {
    if (pays.isEmpty) {
      1.0
    } else {
      (pays filter (_.status == ScheduledPayStatus.FULFILLED) map (_.sum)).sum / pays.map(_.sum).sum
    }
  }

}
