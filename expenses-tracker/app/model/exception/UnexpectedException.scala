package model.exception

final case class UnexpectedException(field: String, message: String) extends FieldSpecifiedError
