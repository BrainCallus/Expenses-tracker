package util

import model.util.DateUtil.DateCalc
import model.util.DateUtil

import java.time.LocalDate

object CommonTestUtils {
  val defaultPassword = "12345"

  def getTomorrow: LocalDate = 1.daysLater(getToday)
  def getToday: LocalDate = DateUtil.getNow.toLocalDate
  def getYesterday: LocalDate = 1.daysBefore(getToday)
}
