package controllers

import util.CommonTestUtils.defaultPassword

import org.scalatest.BeforeAndAfterAll
import org.scalatestplus.play._
import org.scalatestplus.play.guice._
import play.api.Play.materializer
import play.api.test._
import play.api.test.Helpers._
import play.api.test.CSRFTokenHelper._

class LoginControllerSpec extends PlaySpec with BeforeAndAfterAll with GuiceOneAppPerTest with Injecting {

  private val login = "testUser"

  "LoginController initTest" should {
    "create new user that previously didn't exist" in {
      base.BaseTests.createNewUser(login)
    }
  }

  "LoginController GET" should {

    "render the login page from a new instance of controller" in {
      val controller = new LoginController(stubControllerComponents())
      val loginPage = controller.renderLoginForm().apply(FakeRequest(GET, "/login").withCSRFToken)

      status(loginPage) mustBe OK
      contentType(loginPage) mustBe Some("text/html")
      contentAsString(loginPage) must include("<form id=\"enterForm\" method=\"post\" action=\"/loginUser\">")
    }
  }

  "LoginController loginUser" should {
    "authenticate user with correct credentials" in {
      val controller = new LoginController(stubControllerComponents())
      val result = controller
        .loginUser()
        .apply(
          FakeRequest(POST, "/loginUser")
            .withFormUrlEncodedBody(
              "login" -> login,
              "password" -> defaultPassword
            )
            .withCSRFToken
        )
      status(result) mustBe SEE_OTHER
      session(result).get("userName") mustBe Some(login)
    }

    "redirect to login with flash message for invalid credentials" in {
      val controller = new LoginController(stubControllerComponents())
      val invalidLogin = controller
        .loginUser()
        .apply(
          FakeRequest(POST, "/loginUser")
            .withFormUrlEncodedBody(
              "login" -> "userNotExist",
              "password" -> defaultPassword
            )
            .withCSRFToken
        )
      status(invalidLogin) mustBe SEE_OTHER
      flash(invalidLogin).get("password") mustBe Some("Invalid login or password")
      val invalidPassword = controller
        .loginUser()
        .apply(
          FakeRequest(POST, "/loginUser")
            .withFormUrlEncodedBody(
              "login" -> "userNotExist",
              "password" -> "invPass"
            )
            .withCSRFToken
        )
      status(invalidPassword) mustBe SEE_OTHER
      flash(invalidPassword).get("password") mustBe Some("Invalid login or password")
    }

    "redirect to login with flash message for empty fields" in {
      val controller = new LoginController(stubControllerComponents())
      val invalidLogin = controller
        .loginUser()
        .apply(
          FakeRequest(POST, "/loginUser")
            .withFormUrlEncodedBody(
              "login" -> "",
              "password" -> defaultPassword
            )
            .withCSRFToken
        )
      status(invalidLogin) mustBe SEE_OTHER
      flash(invalidLogin).get("login") mustBe Some("Field shouldn't be empty")
      val invalidPassword = controller
        .loginUser()
        .apply(
          FakeRequest(POST, "/loginUser")
            .withFormUrlEncodedBody(
              "login" -> "userNotExist",
              "password" -> ""
            )
            .withCSRFToken
        )
      status(invalidPassword) mustBe SEE_OTHER
      flash(invalidPassword).get("password") mustBe Some("Field shouldn't be empty")
    }

  }

  "LoginController logout" should {
    "logout user cleaning session" in {
      val controller = new LoginController(stubControllerComponents())
      val result = controller.logout().apply(FakeRequest(GET, "/logout").withSession("userId" -> "1"))

      status(result) mustBe SEE_OTHER
      session(result).get("userId") mustBe None
    }
  }

  "LoginController finalizeTest" should {
    "remove previously created user if it exist" in {
      base.BaseTests.removeUserByLogin(login)
    }
  }
}
