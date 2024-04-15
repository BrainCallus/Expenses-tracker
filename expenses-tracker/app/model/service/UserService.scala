package model.service

import cats.Monad
import cats.data.EitherT
import cats.effect.Resource
import cats.effect.kernel.MonadCancelThrow
import model.codecs.{Json, JsonWriter}
import model.dao.algebr.UserProvider
import model.dao.io.DbIOProvider
import model.entity.user.{UserWithId, UserWithPassword}
import model.exception.{DBException, FieldSpecifiedError}
import model.util.DBUtils.{DaoOptions, getPasswordEncrypted}
import model.validation.BaseValidatorsLib._
import model.validation.ValidationResult.{Success, ValidationError}
import model.validation.Validator
import play.api.mvc.{AnyContent, Request}

import java.io.IOException

trait UserService[F[_]] {
  def validateRegisterForm(request: Request[AnyContent]): EitherT[F, FieldSpecifiedError, UserWithId]
  def validateLoginForm(request: Request[AnyContent]): EitherT[F, FieldSpecifiedError, UserWithId]
}

object UserService {
  def make[F[_]: Monad](implicit
    commonService: CommonService[F],
    userProvider: Resource[F, UserProvider[F]],
    F: MonadCancelThrow[F]
  ): UserService[F] = new UserService[F] {
    override def validateRegisterForm(request: Request[AnyContent]): EitherT[F, FieldSpecifiedError, UserWithId] = {
      commonService.formRequest(request)(requestParams =>
        commonService.withValidation[UserWithPassword, UserWithId](requestParams, validatorsForRegister)(user =>
          insertAndReturnUser(user)
        )
      )
    }

    override def validateLoginForm(request: Request[AnyContent]): EitherT[F, FieldSpecifiedError, UserWithId] = {
      commonService.formRequest(request)(requestParams =>
        commonService.withValidation[UserWithId, UserWithId](
          requestParams ++ Map("name" -> Seq(""), "id" -> Seq("")),
          validatorsForLogin(requestParams)
        )(EitherT.rightT[F, FieldSpecifiedError](_))
      )
    }

    private def insertAndReturnUser(user: UserWithPassword): EitherT[F, FieldSpecifiedError, UserWithId] = {
      for {
        _ <- EitherT.apply(
          F.flatMap(F.attempt(userProvider.use(_.insert(user))))(e =>
            F.pure(e.fold(_ => Left(DBException("general", "Failed to save new user")), Right(_)))
          )
        )
        userWithIdOpt <- EitherT.right(userProvider.use(_.findByLogin(user.login)))
        userWithId <- EitherT.fromEither(
          userWithIdOpt.toRight[FieldSpecifiedError](DBException("general", "Failed to save new user"))
        )
      } yield userWithId
    }

    private val validatorsForRegister = List(
      (
        "login",
        toOptionJsonValidator(
          LengthValidator(4, 32).>>!((value: String) =>
            DbIOProvider.userProvider.use(_.findByLogin(value)).safeRunToOption match {
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
          DbIOProvider.userProvider
            .use(
              _.findByLoginAndPassword(
                requestParams("login").head,
                getPasswordEncrypted(requestParams("password").head)
              )
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

}
