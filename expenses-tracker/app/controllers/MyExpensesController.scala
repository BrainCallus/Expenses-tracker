package controllers

import cats.effect.unsafe.implicits.global
import com.google.inject.Inject
import doobie.{Read, Write}
import model.util.DateUtil.DateCalc
import model.codecs.JsonReader
import model.dao.algebr.PayTypeProvider
import model.dao.algebr.PayTypeProvider.FullExpenseOrPayEvidence
import model.dao.io.ExpenseDao
import model.entity.DatabaseEntity
import model.entity.pays._
import model.service.{ExpenseService, ForecastService}
import model.util.DateUtil
import play.api.mvc.{AbstractController, ControllerComponents}
import model.validation.ValidationResult.ValidationError
import play.api.libs.json.{JsError, JsSuccess, Json, Reads}
import play.api.mvc._

class MyExpensesController @Inject() (cc: ControllerComponents) extends AbstractController(cc) {
  def view(): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    request.session.get("userId") map { id =>
      val all = ExpenseDao
        .findForPeriod[ExpenseFull](
          id.toLong,
          3.monthBefore(DateUtil.getNow.toLocalDate),
          DateUtil.getNow.toLocalDate,
          isExpense = true
        )
        .unsafeRunSync()
      Ok(
        views.html.myExpenses(
          all,
          ExpenseService.getPastTermScheduledPays(id.toLong)
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
      ForecastService.getMonthForecast(id) match {
        case Left(err)    => Ok(Json.toJson(Map("success" -> "", "error" -> err.message)))
        case Right(value) => Ok(Json.toJson(Map("success" -> 1.0.doubleValue, "predicted" -> value)))
      }
    }
  }

  def changeScheduledStatus(): Action[AnyContent] = Action { implicit request =>
    withJsonBody[(Long, Long, String)] { payInfo =>
      ScheduledPayStatus.withNameInsensitiveOption(payInfo._3) match {
        case None => Ok(Json.toJson(Map("success" -> "", "error" -> "Invalid value: confirm or decline expected")))
        case Some(status) =>
          ExpenseService.updateScheduledStatus(payInfo._2, status, payInfo._1) match {
            case Right(_)              => Ok(Json.toJson(Map("success" -> true)))
            case Left(validationError) => Ok(Json.toJson(Map("success" -> "", "error" -> validationError.message)))
          }
      }
    }

  }

  private def deleteExpenseOrScheduledPay[ExpenseOrPay <: DatabaseEntity with PayType: JsonReader: Read](
    isExpense: Boolean
  )(implicit request: Request[AnyContent], ev: FullExpenseOrPayEvidence[ExpenseOrPay]): Result = {
    withJsonBody[(Long, Long)] { ids =>
      ExpenseService.deleteExpenseOrScheduledPay[ExpenseOrPay](ids._1, ids._2, isExpense) match {
        case Left(ValidationError(_, message)) => Ok(Json.toJson(Map("success" -> "", "error" -> message)))
        case Right(_)                          => Ok(Json.toJson(Map("success" -> true)))
      }

    }
  }

  private def addExpenseOrScheduledPay[ExpenseOrPay <: PayType: Write: JsonReader](
    isExpense: Boolean
  )(implicit request: Request[AnyContent], ev: PayTypeProvider.RawExpenseOrPayEvidence[ExpenseOrPay]): Result = {
    withJsonBody[(String, String, String, String)] { fields =>
      ExpenseService.addExpenseOrSchedulePay[ExpenseOrPay](
        Map(
          "sum" -> Seq(fields._2),
          "expenseType" -> Seq(fields._3),
          "userId" -> Seq(fields._1),
          "date" -> Seq(fields._4)
        ),
        isExpense
      ) match {
        case Left(ValidationError(field, message)) =>
          Ok(Json.toJson(Map("success" -> "", "errorField" -> field, "errorMessage" -> message)))
        case Right(_) => Ok(Json.toJson(Map("success" -> true)))
      }
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
