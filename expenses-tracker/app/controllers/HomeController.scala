package controllers

import com.google.inject.Inject
import play.api.mvc._

class HomeController @Inject() (cc: ControllerComponents) extends AbstractController(cc) {
  def index(): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    Ok(views.html.index())
  }
}
