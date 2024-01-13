package model.dao.algebr

import cats.effect.MonadCancelThrow
import cats.implicits.toFunctorOps
import doobie.implicits._
import doobie.util.transactor.Transactor
import model.dao.algebr.Provider.transact
import model.entity.user.{UserWithId, UserWithPassword}

trait UserProvider[F[_]] extends Provider[F] {
  def findByLogin(login: String): F[Option[UserWithId]]
  def findByLoginAndPassword(login: String, password: String): F[Option[UserWithId]]
  def insert(user: UserWithPassword): F[Unit]
}

object UserProvider {
  def make[F[_]: MonadCancelThrow](xa: Transactor[F]): UserProvider[F] = {
    new UserProvider[F] {
      override def findByLogin(login: String): F[Option[UserWithId]] =
        transact(xa)(sql"""SELECT id, login, name FROM "user" WHERE login=$login """.query[UserWithId].option)

      override def findByLoginAndPassword(login: String, password: String): F[Option[UserWithId]] =
        transact(xa)(
          sql"""SELECT id, login, name FROM "user" WHERE login=$login AND password = $password"""
            .query[UserWithId]
            .option
        )

      override def insert(user: UserWithPassword): F[Unit] =
        transact(xa)(
          sql"""INSERT INTO "user" ("login", "name", "password", "creationtime") VALUES (${user.login}, ${user.name}, ${user.password}, NOW())""".update.run.void
        )
    }
  }
}
