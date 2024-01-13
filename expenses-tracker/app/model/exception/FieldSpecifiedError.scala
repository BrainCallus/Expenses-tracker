package model.exception

trait FieldSpecifiedError extends Throwable {
  def field: String
  def message: String
}
