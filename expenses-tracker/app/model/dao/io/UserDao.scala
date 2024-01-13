package model.dao.io

import cats.effect._
import model.dao.io.DbIOProvider.userOperation
import model.entity.user.{UserWithId, UserWithPassword}

object UserDao {

  def findByLoginAndPassword(login: String, password: String): IO[Option[UserWithId]] = {
    userOperation[Option[UserWithId]](_.findByLoginAndPassword(login, password))
  }

  def insert(user: UserWithPassword): IO[Unit] = {
    userOperation(_.insert(user))

  }

  def findByLogin(login: String): IO[Option[UserWithId]] = {
    userOperation(_.findByLogin(login))
  }
}
