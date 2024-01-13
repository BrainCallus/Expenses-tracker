package model.exception

case class DBException(field: String, message: String) extends FieldSpecifiedError
