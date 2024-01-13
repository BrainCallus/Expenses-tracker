package model.validation

import cats.Monad
import model.exception.FieldSpecifiedError

sealed trait ValidationResult[+T]

object ValidationResult {
  final case class ValidationError(field: String, message: String)
    extends FieldSpecifiedError
    with ValidationResult[Nothing]

  final case class Success[J](result: J) extends ValidationResult[J]

  implicit object MonadValidationResult extends Monad[ValidationResult] {
    override def pure[A](x: A): ValidationResult[A] = Success(x)

    override def flatMap[A, B](fa: ValidationResult[A])(f: A => ValidationResult[B]): ValidationResult[B] =
      fa match {
        case x: ValidationError => x
        case Success(y)         => f(y)
      }

    override def tailRecM[A, B](a: A)(f: A => ValidationResult[Either[A, B]]): ValidationResult[B] =
      fromEitherResult(f(a))

    def tailRecABC[A, B, C](a: A)(f: A => ValidationResult[Either[B, C]]): ValidationResult[C] =
      fromEitherResult(f(a))

    private def fromEitherResult[A, B](result: ValidationResult[Either[A, B]]): ValidationResult[B] =
      result match {
        case x: ValidationError    => x
        case Success(Left(value))  => ValidationError("", value.toString)
        case Success(Right(value)) => Success(value)
      }
  }

}
