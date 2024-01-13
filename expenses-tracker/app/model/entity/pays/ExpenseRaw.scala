package model.entity.pays

import doobie.Write
import model.util.DateUtil.dateToDBString
import model.codecs.JsonReader._
import model.codecs._

import java.time.{LocalDate, LocalDateTime}

final case class ExpenseRaw(sum: Double, expenseType: ExpensesType, userId: Long, date: LocalDate) extends Expense

object ExpenseRaw {
  implicit def rawExpenseReader: JsonReader[ExpenseRaw] = objectReader(map =>
    for {
      sum <- getField[Double](map, "sum")
      expenseType <- getField[ExpensesType](map, "expenseType")
      userId <- getField[Long](map, "userId")
      date <- getField[LocalDateTime](map, "date").map(_.toLocalDate)
    } yield ExpenseRaw(sum, expenseType, userId, date)
  )


  implicit val writeRawExpense: Write[ExpenseRaw] =
    Write[(Double, String, Long, String)].contramap { expense =>
      (expense.sum, expense.expenseType.toString, expense.userId, dateToDBString(expense.date))
    }
}
