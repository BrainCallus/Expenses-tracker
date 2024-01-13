package model.dao.algebr

import cats.effect.kernel.MonadCancelThrow
import doobie.Read
import doobie.implicits._
import doobie.util.transactor.Transactor
import doobie.util.update.Update
import model.util.DateUtil.dateToDBString
import Provider.transact
import cats.implicits.toFunctorOps
import model.dao.algebr.PayTypeProvider.{FullExpenseOrPayEvidence, V}
import model.entity.DatabaseEntity
import model.entity.pays._

import java.time.LocalDate

trait PayTypeProvider[F[_]] extends Provider[F] {

  def find[T: Read](id: Long, isExpense: Boolean)(implicit ev: FullExpenseOrPayEvidence[T]): F[Option[T]]
  def insert[T](entity: T)(implicit ev: V[ExpenseRaw, ScheduledPayRaw, T]): F[Unit]
  def delete[T <: DatabaseEntity](entity: T)(implicit ev: FullExpenseOrPayEvidence[T]): F[Unit]
  def findByUser[T: Read](userId: Long, isExpense: Boolean)(implicit ev: FullExpenseOrPayEvidence[T]): F[List[T]]
  def findByPeriod[T: Read](userId: Long, start: LocalDate, end: LocalDate, isExpense: Boolean)(implicit
    ev: FullExpenseOrPayEvidence[T]
  ): F[List[T]]
}

object PayTypeProvider {
  trait Or[A, B]

  object Or {
    private val evidence: Or[Any, Any] = new Object with Or[Any, Any]

    implicit def aExistsEv[A, B](implicit a: A): Or[A, B] =
      evidence.asInstanceOf[Or[A, B]]

    implicit def bExistsEv[A, B](implicit b: B): Or[A, B] =
      evidence.asInstanceOf[Or[A, B]]
  }
  type V[A, B, T] = (T <:< A) Or (T <:< B)
  type RawExpenseOrPayEvidence[T] = V[ExpenseRaw, ScheduledPayRaw, T]
  type FullExpenseOrPayEvidence[T] = V[ExpenseFull, ScheduledPayFull, T]

  def make[F[_]: MonadCancelThrow](xa: Transactor[F]): PayTypeProvider[F] = {
    new PayTypeProvider[F] {

      override def find[T: Read](id: Long, isExpense: Boolean)(implicit ev: FullExpenseOrPayEvidence[T]): F[Option[T]] =
        transact(xa)(
          (fr"""SELECT * FROM """ ++ (if (isExpense) fr"""expense""" else fr"""scheduledPay""")
            ++ fr""" WHERE id=$id""").query[T].option
        )

      override def insert[T](entity: T)(implicit ev: RawExpenseOrPayEvidence[T]): F[Unit] = transact(xa)(entity match {
        case pay: ScheduledPayRaw =>
          Update[ScheduledPayRaw](
            "INSERT INTO \"scheduledpay\"" +
              "(\"sum\", \"expensetype\", \"userid\", \"date\",\"status\", \"creationtime\") VALUES (?,?,?,?::date,?, NOW())"
          )
            .updateMany(List(pay))
            .void
        case expense: ExpenseRaw =>
          Update[ExpenseRaw](
            "INSERT INTO \"expense\"" +
              "(\"sum\", \"expensetype\", \"userid\", \"date\", \"creationtime\") VALUES (?,?,?,?::date, NOW())"
          )
            .updateMany(List(expense))
            .void
      })

      override def delete[T <: DatabaseEntity](
        entity: T
      )(implicit ev: (T <:< ExpenseFull) Or (T <:< ScheduledPayFull)): F[Unit] =
        transact(xa)(
          (fr"""DELETE FROM """ ++
            (entity match {
              case _: ScheduledPayFull => fr"""scheduledPay"""
              case _: ExpenseFull      => fr"""expense"""
            }) ++ fr""" WHERE id=${entity.id}""").update.run.void
        )

      override def findByUser[T: Read](userId: Long, isExpense: Boolean)(implicit
        ev: FullExpenseOrPayEvidence[T]
      ): F[List[T]] =
        transact(xa)(
          (fr"""SELECT * FROM """ ++ (if (isExpense) fr"""expense""" else fr"""scheduledPay""")
            ++ fr""" WHERE userid=$userId ORDER BY date DESC""").query[T].to[List]
        )

      override def findByPeriod[T: Read](userId: Long, start: LocalDate, end: LocalDate, isExpense: Boolean)(implicit
        ev: FullExpenseOrPayEvidence[T]
      ): F[List[T]] = transact(xa)(
        (fr"""SELECT * FROM """ ++ (if (isExpense) fr"""expense""" else fr"""scheduledPay""")
          ++ fr""" WHERE userid=$userId AND date between ${dateToDBString(start)}::date AND ${dateToDBString(
            end
          )}::date ORDER BY date DESC""")
          .query[T]
          .to[List]
      )
    }
  }
}
