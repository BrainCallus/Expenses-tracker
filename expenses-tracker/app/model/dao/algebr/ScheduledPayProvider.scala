package model.dao.algebr

import cats.effect.kernel.MonadCancelThrow
import cats.implicits.toFunctorOps
import doobie.implicits._
import doobie.util.transactor.Transactor
import Provider.transact
import model.entity.pays.{ScheduledPayFull, ScheduledPayStatus}

trait ScheduledPayProvider[F[_]] extends Provider[F] {
  def updatePayStatus(pay: ScheduledPayFull, newStatus: ScheduledPayStatus): F[Unit]
}

object ScheduledPayProvider {
  def make[F[_]: MonadCancelThrow](xa: Transactor[F]): ScheduledPayProvider[F] = {
    (pay: ScheduledPayFull, newStatus: ScheduledPayStatus) =>
      transact(xa)(
        sql"""UPDATE scheduledPay SET status=${newStatus.toString.toLowerCase} WHERE id=${pay.id}""".update.run.void
      )
  }
}
