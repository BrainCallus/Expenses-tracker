package model.service

import cats.data.EitherT
import cats.effect.{Concurrent, MonadCancelThrow}
import model.dao.algebr.PayTypeProvider
import model.dao.io.DbIOProvider
import model.dao.io.DbIOProvider.findNow
import model.entity.pays._
import model.entity.useroption._
import model.exception._
import model.py.HttpPyServSarima.logger
import model.py.HttpPyServSarimaAlg
import model.service.CommonService.actionFromOption
import model.util.DateUtil.getNow
import model.validation.ValidationResult.ValidationError

import java.time.{LocalDate, LocalDateTime}

trait ForecastService[F[_]] {
  def monthForecast(userId: Long): EitherT[F, FieldSpecifiedError, Double]
}

object ForecastService {
  def make[F[_]: MonadCancelThrow](implicit payTypeProvider: PayTypeProvider[F], userOptionService: UserOptionService[F], F: Concurrent[F]):ForecastService[F] =
    (userId: Long) => {
      val today = getNow.toLocalDate
      val todayStart = LocalDateTime.of(today.getYear, today.getMonth, today.getDayOfMonth, 0, 0, 1)
      val res = for {
        userOption <- EitherT.right[FieldSpecifiedError](userOptionService.findOption( "forecast", userId))

        res1 <- requestFromPyOnNoneAlg(userOption, userId)(forecastOption => {
          checkTermAndGetForecastAlg(forecastOption, todayStart)(userId, forecastOpt => {
            for {
              lastTimeUpdatedOpt <- EitherT.right[FieldSpecifiedError](userOptionService.findOption("lastTimeUpdated", userId))
              r2 <- lastTimeUpdatedOpt map (lastTimeUpdated => {
                checkTermAndGetForecastAlg(forecastOpt, lastTimeUpdated.updationTime)(userId, (option: UserOptionDB) => {
                  requestFromPyOnNoneAlg(option.value.toDoubleOption, userId)(EitherT.rightT[F, FieldSpecifiedError].apply)
                })
              }) getOrElse EitherT.leftT(ValidationError("error", "User data not found").asInstanceOf[FieldSpecifiedError])
            } yield r2
          })
        })
      } yield res1
      res
    }

  private def requestPredictedFromPy[F[_]: MonadCancelThrow](userId: Long)(implicit expenseProvider: PayTypeProvider[F], userOptionService: UserOptionService[F], F: Concurrent[F]): EitherT[F, FieldSpecifiedError, Double] = {
    val today = DbIOProvider.findNow().toLocalDate
    for {
      requestParams <- EitherT.right[FieldSpecifiedError](expenseProvider.findByUser[ExpenseFull](userId, isExpense = true))
        .map(data => (today.getMonth.length(today.isLeapYear) - today.getDayOfMonth, ExpenseService.getSumsByDay(data)))
      httpServ = HttpPyServSarimaAlg.make[F]
      prediction <- for {
        task <-httpServ.buildTask(requestParams._1, requestParams._2)
        pyPredictions <-  EitherT.apply(
          httpServ.runOnCancel(F.pure(task))
        )

        expectedForScheduled <- expectedSumForScheduledAlg(userId)
        curMonthExpenses = getExpensesFromCurrentMonth(requestParams._2, today)
        forecast = expectedForScheduled + pyPredictions.sum + curMonthExpenses
        _ <- EitherT.right[FieldSpecifiedError]( F.redeem(userOptionService.setOption(userId, "forecast", forecast.toString))(_ => (), identity))
      } yield forecast
    } yield prediction
  }

  private def checkTermAndGetForecastAlg[F[_]:MonadCancelThrow](forecastOpt: UserOptionDB, deadline: LocalDateTime)(
    userId: Long, action: UserOptionDB => EitherT[F, FieldSpecifiedError, Double])(implicit expenseProvider: PayTypeProvider[F], userOptionService: UserOptionService[F], F: Concurrent[F]):EitherT[F,FieldSpecifiedError,Double] = {
    if ((forecastOpt.updationTime compareTo deadline) >= 0) {
      action(forecastOpt)
    } else {
      requestPredictedFromPy[F](userId)
    }
  }

  private def requestFromPyOnNoneAlg[T, F[_]:MonadCancelThrow](option: Option[T], userId: Long)(
    action: T => EitherT[F, FieldSpecifiedError, Double]
  )(implicit expenseProvider: PayTypeProvider[F], userOptionService: UserOptionService[F], F: Concurrent[F]): EitherT[F, FieldSpecifiedError, Double] = {
    actionFromOption(option)(requestPredictedFromPy(userId))(action(_))
  }

  private def getExpensesFromCurrentMonth(expenses: List[(LocalDate, Double)], today: LocalDate): Double =
    (expenses filter (pair =>
      (pair._1 compareTo LocalDate.of(today.getYear, today.getMonthValue, 1)) >= 0
    ) map (_._2)).sum


  private def expectedSumForScheduledAlg[F[_]:MonadCancelThrow](userId: Long)(implicit payProvider: PayTypeProvider[F], F: Concurrent[F]): EitherT[F, FieldSpecifiedError, Double] = {
    val today = findNow().toLocalDate
    val r = payProvider.findByPeriod[ScheduledPayFull](userId, today, LocalDate.of(
      today.getYear,
      today.getMonth,
      today.getMonth.maxLength()
    ), isExpense = false)
    EitherT.apply(MonadCancelThrow[F].redeem(r)(e=>{
      logger.error(e)("Exception occurred while getting ScheduledPays info")
      Left(DBException("error", "Unable to provide DB operation through occurred exception during it."))
    }, list => {
      val avgC = ExpenseService.getPreviousPays(userId) map (x => {
        val avgComplete = computePercentMoneyComplete(x)
        (list filter (_.status == ScheduledPayStatus.SCHEDULED) map (_.sum)).sum * avgComplete
      })
      avgC
    })
    )
  }

  private def computePercentMoneyComplete(pays: List[ScheduledPayFull]): Double = {
    if (pays.isEmpty) {
      1.0
    } else {
      (pays filter (_.status == ScheduledPayStatus.FULFILLED) map (_.sum)).sum / pays.map(_.sum).sum
    }
  }
}
