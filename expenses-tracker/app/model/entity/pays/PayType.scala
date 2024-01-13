package model.entity.pays

import java.time.LocalDate

trait PayType {
  def sum: Double
  def expenseType: ExpensesType
  def userId: Long
  def date: LocalDate
}
