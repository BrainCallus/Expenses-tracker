package model.dao.io

import cats.effect.IO
import doobie.{Read, Write}
import model.dao.algebr.PayTypeProvider.{FullExpenseOrPayEvidence, RawExpenseOrPayEvidence}
import model.dao.io.DbIOProvider.payTypeOperation
import model.entity.DatabaseEntity
import model.entity.pays.PayType

import java.time.LocalDate

object ExpenseDao {
  def find[ExpenseOrPay <: PayType: Read](id: Long, name: String)(implicit
    ev: FullExpenseOrPayEvidence[ExpenseOrPay]
  ): IO[Option[ExpenseOrPay]] = {
    payTypeOperation(_.find[ExpenseOrPay](id, name == "expense"))
  }

  def insert[ExpenseOrPay: Write](
    entity: ExpenseOrPay
  )(implicit ev: RawExpenseOrPayEvidence[ExpenseOrPay]): IO[Unit] = {
    payTypeOperation(_.insert(entity))
  }

  def delete[ExpenseOrPay <: DatabaseEntity](
    entity: ExpenseOrPay
  )(implicit ev: FullExpenseOrPayEvidence[ExpenseOrPay]): IO[Unit] = {
    payTypeOperation(_.delete(entity))
  }

  def findAllForUser[ExpenseOrPay <: PayType: Read](userId: Long, issExpense: Boolean)(implicit
    ev: FullExpenseOrPayEvidence[ExpenseOrPay]
  ): IO[List[ExpenseOrPay]] = {
    payTypeOperation(_.findByUser[ExpenseOrPay](userId, issExpense))
  }

  def findForPeriod[ExpenseOrPay <: PayType: Read](userId: Long, start: LocalDate, end: LocalDate, isExpense: Boolean)(
    implicit ev: FullExpenseOrPayEvidence[ExpenseOrPay]
  ): IO[List[ExpenseOrPay]] = {
    payTypeOperation(_.findByPeriod[ExpenseOrPay](userId, start, end, isExpense))
  }
}
