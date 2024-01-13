package util.option

import cats.effect.IO
import cats.implicits.toFunctorOps
import doobie.implicits._
import model.dao.io.DbIOProvider.xa
import model.dao.io.UserOptionDao
import model.entity.useroption.UserOptionDB

object TestUserOption {
  def getLastTimeUpdated(userId: Long): UserOptionDB =
    UserOptionDao.findByKeyAndUserId("lastTimeUpdated", userId).get

  def dropUserOptions(userId: Long): IO[Unit] =
    sql"""DELETE FROM userOption WHERE userId=$userId""".update.run.void.transact(xa)
}
