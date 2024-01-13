package model.entity.useroption

import java.time.LocalDateTime

trait UserOption {
  def key: String
  def value: String
  def userId: Long
  def updationTime: LocalDateTime
}
