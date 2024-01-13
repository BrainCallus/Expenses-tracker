package model.entity.pays

import doobie.util.Read
import model.codecs.JsonReader
import model.codecs.JsonReader.{getField, objectReader}
import model.entity.DatabaseEntity
import model.util.DateUtil

import java.time.{LocalDate, LocalDateTime}

final case class ScheduledPayFull(
  id: Long,
  sum: Double,
  expenseType: ExpensesType,
  userId: Long,
  date: LocalDate,
  status: ScheduledPayStatus,
  creationTime: LocalDate
) extends ScheduledPay
  with DatabaseEntity

object ScheduledPayFull {
  implicit def fullScheduledPayReader: JsonReader[ScheduledPayFull] = objectReader(map =>
    for {
      id <- getField[Long](map, "id")
      sum <- getField[Double](map, "sum")
      expenseType <- getField[ExpensesType](map, "expenseType")
      userId <- getField[Long](map, "userId")
      date <- getField[LocalDateTime](map, "date") map (_.toLocalDate)
      status <- getField[ScheduledPayStatus](map, "status")
      creationTime <- getField[LocalDateTime](map, "creationTime") map (_.toLocalDate)
    } yield ScheduledPayFull(id, sum, expenseType, userId, date, status, creationTime)
  )

  implicit val readScheduledPayFull: Read[ScheduledPayFull] =
    Read[(Long, Double, String, Long, String, String, String)].map {
      case (id, sum, expenseType, userId, date, status, creationTime) =>
        ScheduledPayFull(
          id,
          sum,
          ExpensesType.withNameInsensitive(expenseType),
          userId,
          DateUtil.localDateFromStringUnsafe(date),
          ScheduledPayStatus.withNameInsensitive(status),
          DateUtil.localDateFromStringUnsafe(creationTime)
        )
    }
}
