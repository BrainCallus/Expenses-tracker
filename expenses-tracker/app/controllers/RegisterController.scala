package controllers

import com.google.inject.Inject
import model.service.UserService

import play.api.mvc._

class RegisterController @Inject() (cc: ControllerComponents) extends AbstractController(cc) {
  def renderRegisterForm(): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    Ok(views.html.register(""))
  }

  def registerUser: Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    UserService.validateRegisterForm(request) match {
      case Left(value) => Redirect("/register").flashing(value.field -> value.message)
      case Right(_)    => Redirect("/login").flashing("message" -> "Successfully registered")
    }
  }
}
