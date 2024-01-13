package model.entity

import java.time.LocalDate

trait DatabaseEntity {
  def id: Long
  def creationTime: LocalDate
}
