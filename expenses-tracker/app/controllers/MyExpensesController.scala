package controllers

import cats.data.EitherT
import cats.effect.{IO, Resource}
import cats.effect.unsafe.implicits.global
import com.google.inject.Inject
import doobie.{Read, Write}
import model.codecs.JsonReader
import model.dao.algebr.PayTypeProvider
import model.dao.algebr.PayTypeProvider.FullExpenseOrPayEvidence
import model.dao.io.DbIOProvider
import model.entity.DatabaseEntity
import model.entity.pays._
import model.exception.FieldSpecifiedError
import model.service.{ExpenseService, IoImplicits}
import model.util.DateUtil
import model.util.DateUtil.DateCalc
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import play.api.libs.json.{JsError, JsSuccess, Json, Reads}
import play.api.mvc._

class MyExpensesController @Inject() (cc: ControllerComponents) extends AbstractController(cc) {
  implicit val logger: SelfAwareStructuredLogger[IO] = Slf4jLogger.getLogger[IO]
  private val expenseService: ExpenseService[IO] = IoImplicits.expenseService
  private val payTypeProvider: Resource[IO, PayTypeProvider[IO]] = DbIOProvider.payTypeProvider
  def view(): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    request.session.get("userId") map { id =>
      val all = payTypeProvider
        .use(
          _.findByPeriod[ExpenseFull](
            id.toLong,
            3.monthBefore(DateUtil.getNow.toLocalDate),
            DateUtil.getNow.toLocalDate,
            isExpense = true
          )
        )
        .unsafeRunSync()
      val pastPays = expenseService.getPastTermScheduledPays(id.toLong).unsafeRunSync()
      Ok(
        views.html.myExpenses(
          all,
          pastPays
        )
      )
    } getOrElse Redirect("/login").flashing("errorMessage" -> "You're not logged in")
  }

  def addExpense(): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    addExpenseOrScheduledPay[ExpenseRaw](isExpense = true)
  }

  def addSchedulePay(): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    addExpenseOrScheduledPay[ScheduledPayRaw](isExpense = false)
  }

  def deleteExpense(): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    deleteExpenseOrScheduledPay[ExpenseFull](isExpense = true)
  }

  def monthPredict(): Action[AnyContent] = Action { implicit request =>
    withJsonBody[Long] { id =>
      IoImplicits.forecastService
        .monthForecast(id)
        .value
        .map(
          _.fold(
            ex => Ok(Json.toJson(Map("success" -> "", "error" -> ex.message))),
            value => Ok(Json.toJson(Map("success" -> 1.0.doubleValue, "predicted" -> value)))
          )
        )
        .handleErrorWith(err => {
          logger
            .error(err)("Unhandled exception from ForecastService")
            .map(_ => Ok(Json.toJson(Map("success" -> "", "error" -> err.getMessage))))
        })
        .unsafeRunSync()
    }
  }

  //todo: .map(_.fold...) -> remove copy paste
  def changeScheduledStatus(): Action[AnyContent] = Action { implicit request =>
    withJsonBody[(Long, Long, String)] { payInfo =>
      ScheduledPayStatus.withNameInsensitiveOption(payInfo._3) match {
        case None => Ok(Json.toJson(Map("success" -> "", "error" -> "Invalid value: confirm or decline expected")))
        case Some(status) =>
          handleEitherUnitResultAction(
            expenseService
              .updateScheduledStatus(payInfo._2, status, payInfo._1),
            "Unhandled exception while changing pay status"
          )
      }
    }

  }

  private def deleteExpenseOrScheduledPay[ExpenseOrPay <: DatabaseEntity with PayType: JsonReader: Read](
    isExpense: Boolean
  )(implicit request: Request[AnyContent], ev: FullExpenseOrPayEvidence[ExpenseOrPay]): Result = {
    withJsonBody[(Long, Long)] { ids =>
      handleEitherUnitResultAction(
        expenseService
          .deleteExpenseOrScheduledPay[ExpenseOrPay](ids._1, ids._2, isExpense),
        "Unhandled exception while deleting expense"
      )
    }
  }

  private def handleEitherUnitResultAction(
    either: EitherT[IO, FieldSpecifiedError, Unit],
    unhandledMessage: String = ""
  ): Result = {
    either.value
      .map(
        _.fold(
          ex => Ok(Json.toJson(Map("success" -> "", "error" -> ex.message))),
          _ => Ok(Json.toJson(Map("success" -> true)))
        )
      )
      .handleError(err => {
        logger.error(err)(unhandledMessage)
        Ok(Json.toJson(Map("success" -> "", "error" -> err.getMessage)))
      })
      .unsafeRunSync()
  }

  //todo: add Reads[_] for case classes ..Raw
  private def addExpenseOrScheduledPay[ExpenseOrPay <: PayType: Write: JsonReader](
    isExpense: Boolean
  )(implicit request: Request[AnyContent], ev: PayTypeProvider.RawExpenseOrPayEvidence[ExpenseOrPay]): Result = {
    withJsonBody[(String, String, String, String)] { fields =>
      expenseService
        .addExpenseOrSchedulePay[ExpenseOrPay](
          Map(
            "sum" -> Seq(fields._2),
            "expenseType" -> Seq(fields._3),
            "userId" -> Seq(fields._1),
            "date" -> Seq(fields._4)
          ),
          isExpense
        )
        .value
        .map(
          _.fold(
            ex => Ok(Json.toJson(Map("success" -> "", "errorField" -> ex.field, "errorMessage" -> ex.message))),
            _ => Ok(Json.toJson(Map("success" -> true)))
          )
        )
        .handleErrorWith(err => {
          logger
            .error(err)("Unhandled while adding expense")
            .map(_ => Ok(Json.toJson(Map("success" -> "", "error" -> err.getMessage))))
        })
        .unsafeRunSync()
    }
  }

  private def withJsonBody[A](f: A => Result)(implicit request: Request[AnyContent], reads: Reads[A]): Result = {
    request.body.asJson
      .map { body =>
        Json.fromJson[A](body) match {
          case JsSuccess(a, _) => f(a)
          case JsError(err) =>
            println(err)
            Redirect("/my-expenses").flashing("errorMessage" -> "Oops, something went wrong")
        }
      }
      .getOrElse({
        println("body fail")
        Redirect("/my-expenses").flashing("errorMessage" -> "Request is empty")
      })
  }
}
