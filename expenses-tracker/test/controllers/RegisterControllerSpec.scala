package controllers

import cats.effect.unsafe.implicits.global
import util.CommonTestUtils.defaultPassword
import model.dao.io.UserDao
import org.scalatest.BeforeAndAfterAll
import org.scalatestplus.play._
import org.scalatestplus.play.guice._
import play.api.Play.materializer
import play.api.test.CSRFTokenHelper._
import play.api.test.Helpers._
import play.api.test._

class RegisterControllerSpec extends PlaySpec with BeforeAndAfterAll with GuiceOneAppPerTest with Injecting {

  private val login = "testUser"

  "RegisterController initTest" should {
    "create new user that previously didn't exist" in {
      base.BaseTests.createNewUser(login)
    }
  }

  "RegisterController GET" should {

    "render the login page from a new instance of controller" in {
      val controller = new RegisterController(stubControllerComponents())
      val registerPage = controller.renderRegisterForm().apply(FakeRequest(GET, "/login").withCSRFToken)

      status(registerPage) mustBe OK
      contentType(registerPage) mustBe Some("text/html")
      contentAsString(registerPage) must include(
        "<form method=\"post\" action=\"/registerUser\" id=\"registrationForm\">"
      )
    }
  }

  "RegisterController registerUser" should {
    "register user with correct credentials with flash message" in {
      val controller = new RegisterController(stubControllerComponents())
      try {
        val result = controller
          .registerUser()
          .apply(
            FakeRequest(POST, "/registerUser")
              .withFormUrlEncodedBody(
                "login" -> "testReg",
                "name" -> "testRegName",
                "password" -> defaultPassword
              )
              .withCSRFToken
          )
        status(result) mustBe SEE_OTHER
        flash(result).get("message") mustBe Some("Successfully registered")
        val user = UserDao.findByLogin("testReg").unsafeRunSync().get
        user.login mustBe "testReg"
        user.name mustBe "testRegName"
      } finally {
        base.BaseTests.removeUserByLogin("testReg")
      }

    }

    "redirect to register with flash message for empty fields" in {
      val controller = new RegisterController(stubControllerComponents())
      val invalidLogin = controller
        .registerUser()
        .apply(
          FakeRequest(POST, "/registerUser")
            .withFormUrlEncodedBody(
              "login" -> "",
              "name" -> "someName",
              "password" -> defaultPassword
            )
            .withCSRFToken
        )
      status(invalidLogin) mustBe SEE_OTHER

      flash(invalidLogin).get("login") mustBe Some("Field shouldn't be empty")
      val invalidName = controller
        .registerUser()
        .apply(
          FakeRequest(POST, "/registerUser")
            .withFormUrlEncodedBody(
              "login" -> "someLogin",
              "name" -> "",
              "password" -> defaultPassword
            )
            .withCSRFToken
        )
      status(invalidName) mustBe SEE_OTHER
      flash(invalidName).get("name") mustBe Some("Field shouldn't be empty")

      val invalidPassword = controller
        .registerUser()
        .apply(
          FakeRequest(POST, "/registerUser")
            .withFormUrlEncodedBody(
              "login" -> "someLogin",
              "name" -> "someName",
              "password" -> ""
            )
            .withCSRFToken
        )
      status(invalidPassword) mustBe SEE_OTHER
      flash(invalidPassword).get("password") mustBe Some("Field shouldn't be empty")
    }

    "redirect to register with flash message for to short credentials" in {
      val controller = new RegisterController(stubControllerComponents())
      val invalidLogin = controller
        .registerUser()
        .apply(
          FakeRequest(POST, "/registerUser")
            .withFormUrlEncodedBody(
              "login" -> "lol",
              "name" -> "someName",
              "password" -> defaultPassword
            )
            .withCSRFToken
        )
      status(invalidLogin) mustBe SEE_OTHER
      flash(invalidLogin).get("login") mustBe Some("from 4 to 32 symbols expected")

      val invalidName = controller
        .registerUser()
        .apply(
          FakeRequest(POST, "/registerUser")
            .withFormUrlEncodedBody(
              "login" -> "someLogin",
              "name" -> "lol",
              "password" -> defaultPassword
            )
            .withCSRFToken
        )
      status(invalidName) mustBe SEE_OTHER
      flash(invalidName).get("name") mustBe Some("from 4 to 50 symbols expected")

      val invalidPassword = controller
        .registerUser()
        .apply(
          FakeRequest(POST, "/registerUser")
            .withFormUrlEncodedBody(
              "login" -> "someLogin",
              "name" -> "someName",
              "password" -> "123"
            )
            .withCSRFToken
        )
      status(invalidPassword) mustBe SEE_OTHER
      flash(invalidPassword).get("password") mustBe Some("from 4 to 64 symbols expected")
    }

    "redirect to register with flash message for to long credentials" in {
      val controller = new RegisterController(stubControllerComponents())
      val invalidLogin = controller
        .registerUser()
        .apply(
          FakeRequest(POST, "/registerUser")
            .withFormUrlEncodedBody(
              "login" -> "lol".repeat(12),
              "name" -> "someName",
              "password" -> defaultPassword
            )
            .withCSRFToken
        )
      status(invalidLogin) mustBe SEE_OTHER
      flash(invalidLogin).get("login") mustBe Some("from 4 to 32 symbols expected")

      val invalidName = controller
        .registerUser()
        .apply(
          FakeRequest(POST, "/registerUser")
            .withFormUrlEncodedBody(
              "login" -> "someLogin",
              "name" -> "lol".repeat(20),
              "password" -> defaultPassword
            )
            .withCSRFToken
        )
      status(invalidName) mustBe SEE_OTHER
      flash(invalidName).get("name") mustBe Some("from 4 to 50 symbols expected")

      val invalidPassword = controller
        .registerUser()
        .apply(
          FakeRequest(POST, "/registerUser")
            .withFormUrlEncodedBody(
              "login" -> "someLogin",
              "name" -> "someName",
              "password" -> ("12".repeat(32) + "1")
            )
            .withCSRFToken
        )
      status(invalidPassword) mustBe SEE_OTHER
      flash(invalidPassword).get("password") mustBe Some("from 4 to 64 symbols expected")
    }

    "redirect to register with flash message for already used login" in {
      val controller = new RegisterController(stubControllerComponents())
      val invalidLogin = controller
        .registerUser()
        .apply(
          FakeRequest(POST, "/registerUser")
            .withFormUrlEncodedBody(
              "login" -> login,
              "name" -> "someName",
              "password" -> defaultPassword
            )
            .withCSRFToken
        )
      status(invalidLogin) mustBe SEE_OTHER
      flash(invalidLogin).get("login") mustBe Some("Login already in use")
    }
  }

  "RegisterController finalizeTest" should {
    "remove previously created user if it exist" in {
      base.BaseTests.removeUserByLogin(login)
    }
  }

}
