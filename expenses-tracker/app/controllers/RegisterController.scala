package controllers

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.google.inject.Inject
import model.service.IoImplicits.userService
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import play.api.mvc._

class RegisterController @Inject() (cc: ControllerComponents) extends AbstractController(cc) {
  implicit val logger: SelfAwareStructuredLogger[IO] = Slf4jLogger.getLogger[IO]
  def renderRegisterForm(): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    request.session.get("userId") match {
      case Some(_) => Redirect("/")
      case None    => Ok(views.html.register(""))
    }
  }

  def registerUser(): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    userService
      .validateRegisterForm(request)
      .value
      .map(
        _.fold(
          err => Redirect("/register").flashing(err.field -> err.message),
          _ => Redirect("/login").flashing("message" -> "Successfully registered")
        )
      )
      .handleErrorWith(ex => {
        logger
          .error(ex)("Unhandled exception from ForecastService")
          .map(_ => Redirect("/register").flashing("general" -> ex.getMessage))
      })
      .unsafeRunSync()
  }
}
