package model.dao.algebr

import cats.effect.kernel.MonadCancelThrow
import cats.implicits.toFunctorOps
import doobie.Update
import doobie.implicits._
import doobie.util.transactor.Transactor
import model.dao.algebr.Provider.transact
import model.entity.useroption._

trait UserOptionProvider[F[_]] extends Provider[F] {
  def insert(option: UserOptionRaw): F[Unit]
  def update(option: UserOptionDB, newValue: String): F[Unit]
  def findByKeyAndUserId(key: String, userId: Long): F[Option[UserOptionDB]]
}

object UserOptionProvider {
  def make[F[_]: MonadCancelThrow](xa: Transactor[F]): UserOptionProvider[F] = {
    new UserOptionProvider[F] {
      override def insert(option: UserOptionRaw): F[Unit] =
        transact(xa)(
          Update[UserOptionRaw](
            "INSERT INTO \"useroption\" (\"key\", \"value\", \"userid\",\"updationtime\" ,\"creationtime\") VALUES (?,?,?,NOW(),NOW())"
          )
            .updateMany(List(option))
            .void
        )
      override def update(option: UserOptionDB, newValue: String): F[Unit] =
        transact(xa)(
          sql"""UPDATE userOption SET value=$newValue, updationTime=NOW() WHERE id=${option.id}""".update.run.void
        )
      override def findByKeyAndUserId(key: String, userId: Long): F[Option[UserOptionDB]] =
        transact(xa)(
          sql"""SELECT * FROM userOption WHERE "key"=$key AND userid=$userId""".query[UserOptionDB].option
        )
    }
  }
}
