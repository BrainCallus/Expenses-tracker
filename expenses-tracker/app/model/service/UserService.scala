package model.service

import model.codecs.{Json, JsonWriter}
import model.util.DBUtils.{DaoOptions, getPasswordEncrypted}
import model.dao.io.UserDao
import model.entity.user.{UserWithId, UserWithPassword}
import model.exception.{DBException, FieldSpecifiedError}
import model.service.CommonService.{formRequest, withValidation}
import model.validation.ValidationResult.{Success, ValidationError}
import model.validation.Validator
import model.validation.BaseValidatorsLib._
import play.api.mvc.{AnyContent, Request}

object UserService {
  def validateRegisterForm(request: Request[AnyContent]): Either[FieldSpecifiedError, UserWithId] = {
    formRequest[UserWithId](request)(req => {
      withValidation[UserWithPassword, Either[DBException, UserWithId]](req, validatorsForRegister)(
        (user: UserWithPassword) => insertAndReturnUser(user)
      ).joinRight
    })
  }

  private def insertAndReturnUser(user: UserWithPassword): Either[DBException, UserWithId] = {
    val i = for {
      _ <- UserDao.insert(user)
      userWithId <- UserDao.findByLogin(user.login).map(_.toRight(DBException("general", "Failed to save new user")))
    } yield userWithId
    i.toEitherDBException().joinRight
  }

  def validateLoginForm(request: Request[AnyContent]): Either[FieldSpecifiedError, UserWithId] = {
    formRequest(request)(requestParams => {
      withValidation[UserWithId, UserWithId](
        requestParams ++ Map("name" -> Seq(""), "id" -> Seq("")),
        validatorsForLogin(requestParams)
      )(identity)
    })
  }

  private val validatorsForRegister = List(
    (
      "login",
      toOptionJsonValidator(
        LengthValidator(4, 32).>>!((value: String) =>
          UserDao.findByLogin(value).safeRunToOption match {
            case Some(_) => ValidationError("login", "Login already in use")
            case None    => Success(value)
          }
        )
      )
    ),
    ("name", toOptionJsonValidator(LengthValidator(4, 50))),
    (
      "password",
      toOptionJsonValidator(LengthValidator(4, 64).>>!((value: String) => Success(getPasswordEncrypted(value))))
    )
  )
  private def validatorsForLogin(
    requestParams: Map[String, Seq[String]]
  ): List[(String, Validator[Option[String], Json])] = {
    def fictiveFieldValidator[T: JsonWriter](f: UserWithId => T): Validator[Option[String], Json] =
      toOptionJsonValidator((_: String) => {
        UserDao
          .findByLoginAndPassword(
            requestParams("login").head,
            getPasswordEncrypted(requestParams("password").head)
          )
          .safeRunToOption match {
          case None        => ValidationError("password", "Invalid login or password")
          case Some(value) => Success(f(value))
        }
      })
    List(
      ("login", toOptionJsonValidator(NotBlankValidator())),
      ("password", toOptionJsonValidator(NotBlankValidator().mapRes(pas => getPasswordEncrypted(pas)))),
      ("password", fictiveFieldValidator(_.id)),
      ("name", fictiveFieldValidator(_.name)),
      ("id", fictiveFieldValidator(_.id))
    )
  }
}
