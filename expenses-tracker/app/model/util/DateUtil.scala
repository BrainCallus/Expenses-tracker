package model.util

import model.dao.io.DbIOProvider
import java.sql.Timestamp
import java.time.{LocalDate, LocalDateTime, LocalTime}
import scala.annotation.unused

object DateUtil {
  def dateToDBString(date: LocalDate) = s"${date.getMonthValue}-${date.getDayOfMonth}-${date.getYear}"

  @unused
  def dateTimeToDBString(dateTime: LocalDateTime): String =
    Timestamp.valueOf(dateTime).toString

  def dateToStringOrdinal(date: LocalDate): String = date.toString.split("-").reverse.mkString(".")

  @unused
  def fromTimestampStringUnsafe(value: String): LocalDate =
    Timestamp.valueOf(value.substring(0, value.length - 3)).toLocalDateTime.toLocalDate

  def localDateFromStringUnsafe(value: String): LocalDate = {
    val parts = value.split(" ")(0).split("-")
    LocalDate.of(parts(0).toInt, parts(1).toInt, parts(2).toInt)
  }

  def dateTimeFromStringUnsafe(value: String): LocalDateTime = {
    Timestamp.valueOf(value).toLocalDateTime
  }

  def dateToDateTime(date: LocalDate): LocalDateTime =
    LocalDateTime.of(date, LocalTime.of(0, 0, 1))

  def getNow: LocalDateTime = DbIOProvider.findNow()

  implicit class DateCalc(n: Int) {

    @unused
    def yearsLater(date: LocalDate): LocalDate =
      date.plusYears(n)

    @unused
    def yearsBefore(date: LocalDate): LocalDate =
      date.minusYears(n)

    def monthLater(date: LocalDate): LocalDate = {
      date.plusMonths(n)
    }

    def monthBefore(date: LocalDate): LocalDate = {
      date.minusMonths(n)
    }

    def daysLater(date: LocalDate): LocalDate = {
      date.plusDays(n)
    }

    def daysBefore(date: LocalDate): LocalDate = {
      date.minusDays(n)
    }
  }
}
