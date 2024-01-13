package model.exception

case class PyException(field: String, message: String) extends FieldSpecifiedError
