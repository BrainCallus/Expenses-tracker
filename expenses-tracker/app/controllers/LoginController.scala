package controllers

import com.google.inject.Inject
import model.service.UserService
import play.api.mvc.{AbstractController, Action, AnyContent, ControllerComponents, Request}

class LoginController @Inject() (cc: ControllerComponents) extends AbstractController(cc) {
  def renderLoginForm(): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    Ok(views.html.login())
  }

  def loginUser: Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    UserService.validateLoginForm(request) match {
      case Left(value) => Redirect("/login").flashing(value.field -> value.message)
      case Right(user) =>
        Redirect("/")
          .withSession(
            "userId" -> user.id.toString,
            "userName" -> user.name,
            "csrfToken" -> play.filters.csrf.CSRF.getToken.get.value
          )
          .flashing("message" -> s"Welcome, ${user.name}")
    }
  }

  def logout: Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    Redirect("/").withNewSession
  }
}
