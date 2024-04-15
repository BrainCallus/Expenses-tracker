package model.service

import cats.data.EitherT
import cats.effect.kernel.MonadCancelThrow
import model.codecs.Json.JsonObject
import model.codecs.JsonReader.JsonReaderOps
import model.codecs.{Json, JsonReader}
import model.exception.{FieldSpecifiedError, UnexpectedException}
import model.validation.{Validation, Validator}
import play.api.mvc.{AnyContent, Request}

trait CommonService[F[_]] {
  def withValidation[T: JsonReader, R](
    requestParams: Map[String, Seq[String]],
    validatorMap: List[(String, Validator[Option[String], Json])]
  )(onSuccess: T => EitherT[F, FieldSpecifiedError, R]): EitherT[F, FieldSpecifiedError, R]
  def formRequest[R](request: Request[AnyContent])(
    onBody: Map[String, Seq[String]] => EitherT[F, FieldSpecifiedError, R]
  ): EitherT[F, FieldSpecifiedError, R]
  def actionFromOption[T, R](option: Option[T])(onNone: => R)(onSome: T => R): R =
    CommonService.actionFromOption(option)(onNone)(onSome)
}

object CommonService {
  def make[F[_]: MonadCancelThrow](): CommonService[F] = new CommonService[F] {
    override def withValidation[T: JsonReader, R](
      requestParams: Map[String, Seq[String]],
      validatorMap: List[(String, Validator[Option[String], Json])]
    )(onSuccess: T => EitherT[F, FieldSpecifiedError, R]): EitherT[F, FieldSpecifiedError, R] = {
      for {
        mp <- EitherT.fromEither[F](Validation.validateFields(requestParams)(validatorMap))
        jsonValue <- EitherT.fromEither(
          JsonObject(mp).as[T].fold(_ => Left(UnexpectedException("general", "Unexpected error, try again")), Right(_))
        )
        r <- onSuccess(jsonValue)
      } yield r
    }

    override def formRequest[R](
      request: Request[AnyContent]
    )(onBody: Map[String, Seq[String]] => EitherT[F, FieldSpecifiedError, R]): EitherT[F, FieldSpecifiedError, R] = {
      for {
        requestBody <- EitherT.fromEither(
          request.body.asFormUrlEncoded.toRight(UnexpectedException("general", "Empty request"))
        )
        res <- onBody(requestBody)
      } yield res
    }
  }

  def actionFromOption[T, R](option: Option[T])(onNone: => R)(onSome: T => R): R = {
    option match {
      case None        => onNone
      case Some(value) => onSome(value)
    }
  }
}
