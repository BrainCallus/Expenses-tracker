package model.dao.io

import cats.effect.IO
import model.util.DBUtils.DaoOptions
import model.dao.io.DbIOProvider.userOptionOperation
import model.entity.useroption._

object UserOptionDao {
  def insert(option: UserOptionRaw): IO[Unit] = {
    userOptionOperation(_.insert(option))
  }

  def findByKeyAndUserId(key: String, userId: Long): Option[UserOptionDB] = {
    userOptionOperation(_.findByKeyAndUserId(key, userId)).safeRunToOption
  }

  def update(option: UserOptionDB, newValue: String): IO[Unit] = {
    userOptionOperation(_.update(option, newValue))
  }
}
