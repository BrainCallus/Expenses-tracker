package controllers

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.google.inject.Inject
import model.service.IoImplicits._
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import play.api.mvc._

class LoginController @Inject() (cc: ControllerComponents) extends AbstractController(cc) {
  implicit val logger: SelfAwareStructuredLogger[IO] = Slf4jLogger.getLogger[IO]
  def renderLoginForm(): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    request.session.get("userId") match {
      case Some(_) => Redirect("/")
      case None    => Ok(views.html.login())
    }
  }

  def loginUser(): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    userService
      .validateLoginForm(request)
      .value
      .map(
        _.fold(
          err => Redirect("/login").flashing(err.field -> err.message),
          user => {
            Redirect("/")
              .withSession(
                "userId" -> user.id.toString,
                "userName" -> user.name,
                "csrfToken" -> play.filters.csrf.CSRF.getToken.get.value
              )
              .flashing("message" -> s"Welcome, ${user.name}")
          }
        )
      )
      .handleErrorWith(ex => {
        logger
          .error(ex)("Unhandled exception from UserService")
          .map(_ => Redirect("/login").flashing("general" -> ex.getMessage))
      })
      .unsafeRunSync()
  }

  def logout(): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    Redirect("/").withNewSession
  }
}
