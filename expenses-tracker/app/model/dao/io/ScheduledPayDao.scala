package model.dao.io

import cats.effect.IO
import model.dao.io.DbIOProvider.scheduledPayOperation
import model.entity.pays.{ScheduledPayFull, ScheduledPayStatus}

object ScheduledPayDao {
  def updatePayStatus(pay: ScheduledPayFull, newStatus: ScheduledPayStatus): IO[Unit] = {
    scheduledPayOperation(_.updatePayStatus(pay, newStatus))
  }
}
