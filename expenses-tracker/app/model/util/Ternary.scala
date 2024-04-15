package model.util

object Ternary {
  implicit class Ternary[T](val condition: Boolean) {
    def ??(onTrue: => T, onFalse: => T): T = if (condition) onTrue else onFalse
  }
}
