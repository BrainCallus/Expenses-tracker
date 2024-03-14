package model.service

import model.codecs.Json.JsonObject
import model.codecs.JsonReader.JsonReaderOps
import model.codecs.{Json, JsonReader}
import model.exception.{FieldSpecifiedError, UnexpectedException}
import model.validation.{Validation, Validator}
import play.api.mvc.{AnyContent, Request}

object CommonService {
  def withValidation[T: JsonReader, R](
    requestParams: Map[String, Seq[String]],
    validatorMap: List[(String, Validator[Option[String], Json])]
  )(
    onSuccess: T => R
  ): Either[FieldSpecifiedError, R] = {
    Validation.validateFields(requestParams)(validatorMap) match {
      case Left(value) => Left(value)
      case Right(map) =>
        JsonObject(map).as[T] match {
          case Left(_)      => Left(UnexpectedException("general", "Unexpected error, try again"))
          case Right(value) => Right(onSuccess(value))
        }
    }
  }

  def formRequest[R](
    req: Request[AnyContent]
  )(onBody: Map[String, Seq[String]] => Either[FieldSpecifiedError, R]): Either[FieldSpecifiedError, R] =
    actionFromOption[Map[String, Seq[String]], Either[FieldSpecifiedError, R]](req.body.asFormUrlEncoded)(
      Left(UnexpectedException("general", "Empty request"))
    )(onBody)

  def actionFromOption[T, R](option: Option[T])(onNone: => R)(onSome: T => R): R = {
    option match {
      case None        => onNone
      case Some(value) => onSome(value)
    }
  }
}
