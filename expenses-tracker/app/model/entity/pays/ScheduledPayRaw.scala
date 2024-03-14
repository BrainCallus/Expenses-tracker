package model.entity.pays

import doobie.Write
import model.util.DateUtil.dateToDBString
import model.codecs.JsonReader
import model.codecs.JsonReader.{getField, getOptionField}
import model.entity.pays.ScheduledPayStatus.SCHEDULED
import model.codecs.JsonReader._

import java.time.{LocalDate, LocalDateTime}

final case class ScheduledPayRaw(
  sum: Double,
  expenseType: ExpensesType,
  userId: Long,
  date: LocalDate,
  status: ScheduledPayStatus = SCHEDULED
) extends ScheduledPay

object ScheduledPayRaw {
  def apply(
    sum: Double,
    expenseType: ExpensesType,
    userId: Long,
    date: LocalDate,
    optionStatus: Option[ScheduledPayStatus]
  ): ScheduledPayRaw =
    optionStatus match {
      case None         => new ScheduledPayRaw(sum, expenseType, userId, date)
      case Some(status) => new ScheduledPayRaw(sum, expenseType, userId, date, status)
    }

  implicit def rawScheduledPayReader: JsonReader[ScheduledPayRaw] = objectReader(map =>
    for {
      sum <- getField[Double](map, "sum")
      expenseType <- getField[ExpensesType](map, "expenseType")
      userId <- getField[Long](map, "userId")
      date <- getField[LocalDateTime](map, "date").map(_.toLocalDate)
      status <- getOptionField[ScheduledPayStatus](map, "status")
    } yield ScheduledPayRaw(sum, expenseType, userId, date, status)
  )

  implicit val writeRaeExpense: Write[ScheduledPayRaw] =
    Write[(Double, String, Long, String, String)].contramap { pay =>
      (pay.sum, pay.expenseType.toString, pay.userId, dateToDBString(pay.date), pay.status.toString)
    }
}
