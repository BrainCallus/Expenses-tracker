package model.entity.pays

import doobie.Read
import model.codecs.JsonReader
import model.codecs.JsonReader.{getField, objectReader}
import model.entity.DatabaseEntity
import model.util.DateUtil

import java.time.{LocalDate, LocalDateTime}

final case class ExpenseFull(
  id: Long,
  sum: Double,
  expenseType: ExpensesType,
  userId: Long,
  date: LocalDate,
  creationTime: LocalDate
) extends Expense
  with DatabaseEntity

object ExpenseFull {
  implicit def fullExpenseReader: JsonReader[ExpenseFull] = objectReader(map =>
    for {
      id <- getField[Long](map, "id")
      sum <- getField[Double](map, "sum")
      expenseType <- getField[ExpensesType](map, "expenseType")
      userId <- getField[Long](map, "userId")
      date <- getField[LocalDateTime](map, "date").map(_.toLocalDate)
      creationTime <- getField[LocalDateTime](map, "creationTime").map(_.toLocalDate)
    } yield ExpenseFull(id, sum, expenseType, userId, date, creationTime)
  )

  implicit val readExpenseFull: Read[ExpenseFull] =
    Read[(Long, Double, String, Long, String, String)].map { case (id, sum, expenseType, userId, date, creationTime) =>
      ExpenseFull(
        id,
        sum,
        ExpensesType.withNameInsensitive(expenseType),
        userId,
        DateUtil.localDateFromStringUnsafe(date),
        DateUtil.localDateFromStringUnsafe(creationTime)
      )
    }
}
