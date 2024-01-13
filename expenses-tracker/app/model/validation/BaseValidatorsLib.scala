package model.validation

import model.validation.ValidationResult.{Success, ValidationError}
import model.validation.ValidationResult.MonadValidationResult._
import model.codecs.{Json, JsonWriter}
import model.codecs.JsonWriter.JsonWriterOps
import model.service.CommonService.actionFromOption

import java.util.regex.Pattern
import scala.math.Ordered.orderingToOrdered
import Validator._

import scala.annotation.unused

object BaseValidatorsLib {

  final case class RegexValidator(regex: String) extends Validator[String, String] {
    override def run(value: String): ValidationResult[String] =
      if (Pattern.compile(regex).matcher(value).matches()) Success(value)
      else ValidationError("", "Doesn't satisfy pattern")
  }

  final case class NotBlankValidator() extends Validator[String, String] {
    override def run(value: String): ValidationResult[String] =
      if (value.isBlank) ValidationError("", "Field shouldn't be empty") else Success(value.trim)
  }

  def LengthValidator(minLen: Int, maxLen: Int): Validator[String, String] =
    NotBlankValidator() >>! LenUncheckedValidator(minLen, maxLen)

  def toOptionJsonValidator[T, R: JsonWriter](validator: Validator[T, R]): Validator[Option[T], Json] =
    validatorBimap(optionValidator[T])(validator)(r => r.toJson)

  def optionValidator[A]: Validator[Option[A], A] = (value: Option[A]) =>
    actionFromOption[A, ValidationResult[A]](value)(ValidationError("", "Value is None"))(x => Success(x))

  @unused
  def intValidator(minn: Int, maxx: Int): Validator[String, Int] =
    stringValueValidator(_.toIntOption)(ValueValidator[Int](minn, maxx))

  def longValidator(minn: Long, maxx: Long): Validator[String, Long] =
    stringValueValidator(_.toLongOption)(ValueValidator[Long](minn, maxx))

  def doubleValidator(minn: Double, maxx: Double): Validator[String, Double] =
    stringValueValidator(_.toDoubleOption)(ValueValidator[Double](minn, maxx))

  case class ValueValidator[T](min: T, max: T)(implicit val ordering: Ordering[T]) extends Validator[T, T] {

    override def run(v: T): ValidationResult[T] =
      if (v < min || v > max) ValidationError("", s"Value should be in [$min; $max]")
      else Success(v)
  }

  /** allows to build fromString validator from any ordering value
    */
  @unused
  def stringValueValidator[T](f: String => Option[T])(valueVal: ValueValidator[T]): Validator[String, T] =
    validatorABC((s: String) => pure(f(s)))(optionValidator).>>!(valueVal)

  private final case class LenUncheckedValidator(minLen: Int, maxLen: Int) extends Validator[String, String] {
    override def run(value: String): ValidationResult[String] = if (value.length < minLen || value.length > maxLen)
      ValidationError("", s"from $minLen to $maxLen symbols expected")
    else Success(value)
  }
}
