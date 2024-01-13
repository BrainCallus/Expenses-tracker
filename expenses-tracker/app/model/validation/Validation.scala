package model.validation

import model.codecs.Json
import model.validation.ValidationResult.{Success, ValidationError}

import scala.annotation.tailrec

object Validation {
  def validateFields(request: Map[String, Seq[String]])(
    data: List[(String, Validator[Option[String], Json])]
  ): Either[ValidationError, Map[String, Json]] = runValidation(data, request)()

  @tailrec
  private def runValidation(
    data: List[(String, Validator[Option[String], Json])],
    requestParams: Map[String, Seq[String]]
  )(
    accum: Map[String, Json] = Map.empty[String, Json]
  ): Either[ValidationError, Map[String, Json]] = {
    data match {
      case Nil => Right(accum)
      case head :: tail =>
        head._2.run(requestParams(head._1).headOption) match {
          case ValidationError(_, value) => Left(ValidationError(head._1, value))
          case y: Success[_]             => runValidation(tail, requestParams)(accum + (head._1 -> y.result))
        }
    }
  }
}
