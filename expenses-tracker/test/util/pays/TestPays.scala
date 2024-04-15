package util.pays

import cats.effect.IO
import model.dao.algebr.PayTypeProvider.FullExpenseOrPayEvidence
import model.dao.io.DbIOProvider._
import model.entity.DatabaseEntity
import model.entity.pays._
import model.util.DateUtil.DateCalc
import util.CommonTestUtils.{getToday, getYesterday}
import util.RandomUtil._
import util.account._

import java.time.LocalDate
import scala.util.Random

object TestPays {

  def createUserExpenses(userId: Long, size: Int): IO[Unit] =
    (0 until size).foldLeft(IO.unit)((io: IO[Unit], _: Int) => io.flatMap(_ => createExpense(userId)))

  def createUserFollowScheduledPays(userId: Long, size: Int): IO[Unit] =
    (0 until size).foldLeft(IO.unit)((io: IO[Unit], _: Int) =>
      io.flatMap(_ => createScheduledPay(userId, () => genRandDate(90, 3.monthLater(getToday))))
    )

  def createPastUserScheduledPays(userId: Long, size: Int): IO[Unit] =
    (0 until size).foldLeft(IO.unit)((io: IO[Unit], _: Int) =>
      io.flatMap(_ => createScheduledPay(userId, () => genRandDate(180, getYesterday)))
    )

  def createExpense(userId: Long): IO[Unit] =
    payTypeProvider.use(
      _.insert[ExpenseRaw](
        ExpenseRaw(
          genRandUInt(100, 5000),
          ExpensesType.values(Random.nextInt(ExpensesType.values.length)),
          userId,
          genRandDate(180, getYesterday)
        )
      )
    )

  def createScheduledPay(userId: Long, dateGen: () => LocalDate): IO[Unit] = {
    payTypeProvider.use(
      _.insert[ScheduledPayRaw](
        ScheduledPayRaw(
          genRandUInt(100, 5000),
          ExpensesType.values(Random.nextInt(ExpensesType.values.length)),
          userId,
          dateGen.apply(),
          ScheduledPayStatus.SCHEDULED
        )
      )
    )
  }

  def dropUserExpenses(account: TestUserAccount): IO[Unit] =
    dropPayTypes[ExpenseFull](account, _.expenses)

  def dropUserScheduledPays(account: TestUserAccount): IO[Unit] =
    dropPayTypes[ScheduledPayFull](account, _.scheduledPays)

  private def dropPayTypes[T <: DatabaseEntity](account: TestUserAccount, mapper: AccountWithPays => List[T])(implicit
    ev: FullExpenseOrPayEvidence[T]
  ): IO[Unit] =
    account match {
      case _: BlankUserAccount => IO.unit
      case user: AccountWithPays =>
        mapper(user).foldLeft(IO.unit)((io: IO[Unit], pay: T) => io.flatMap(_ => payTypeProvider.use(_.delete[T](pay))))
    }

  def paysInSegment[T <: PayType](pays: List[T], start: LocalDate, end: LocalDate): List[T] =
    pays filter (pay => pay.date.compareTo(start) >= 0 && pay.date.compareTo(end) <= 0)
}
