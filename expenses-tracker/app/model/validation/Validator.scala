package model.validation

import model.validation.ValidationResult._
import model.validation.ValidationResult.MonadValidationResult.{flatMap, pure}
import scala.annotation.unused

trait Validator[T, R] {
  def run(value: T): ValidationResult[R]
}

object Validator {
  implicit class CompositeValidator[T, E](val validator: Validator[T, E]) {

    /** In case success ignores previous success value and runs on initial
      */
    @unused
    def |>>|[R](other: Validator[T, R]): Validator[T, R] = (value: T) =>
      validator.run(value) match {
        case Success(_)                      => other.run(value)
        case ValidationError(field, message) => ValidationError(field, message)
      }

    /** In case success runs on success value got from previous validator
      */
    def >>![R](other: Validator[E, R]): Validator[T, R] = (value: T) =>
      validator.run(value) match {
        case Success(res)       => other.run(res)
        case x: ValidationError => x
      }

    /** simply applies given function to the result in case success
      */
    def mapRes[B](f: E => B): Validator[T, B] = (value: T) =>
      validator run value match {
        case Success(res)       => Success(f(res))
        case x: ValidationError => x
      }

  }

  implicit def validatorABC[A, B, C](f: A => ValidationResult[B])(validator: Validator[B, C]): Validator[A, C] =
    (value: A) => flatMap(flatMap(pure(value))(f))(validator.run)

  implicit def validatorBimap[A, B, C, D](validatorAB: Validator[A, B])(validatorBC: Validator[B, C])(
    f: C => D
  ): Validator[A, D] =
    validatorABC(validatorAB.run)(validatorBC).mapRes(f)
}
