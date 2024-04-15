package util.option

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.implicits.toFunctorOps
import doobie.implicits._
import model.dao.io.DbIOProvider.{userOptionProvider, xa}
import model.entity.useroption.UserOptionDB

object TestUserOption {
  def getLastTimeUpdated(userId: Long): UserOptionDB =
    userOptionProvider.use(_.findByKeyAndUserId("lastTimeUpdated", userId)).unsafeRunSync().get

  def dropUserOptions(userId: Long): IO[Unit] =
    sql"""DELETE FROM userOption WHERE userId=$userId""".update.run.void.transact(xa)
}
