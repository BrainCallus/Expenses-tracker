package model.service

import cats.effect.MonadCancelThrow
import cats.implicits.{toFlatMapOps, toFunctorOps}
import model.dao.algebr.UserOptionProvider
import model.dao.io.DbIOProvider
import model.entity.useroption.{UserOptionDB, UserOptionRaw}
import model.service.CommonService.actionFromOption

trait UserOptionService[F[_]] {
  def setOption(userId: Long, key: String, value: String): F[Unit]
  def findOption(key: String, userId: Long): F[Option[UserOptionDB]]
}

object UserOptionService {
  def make[F[_]: MonadCancelThrow](implicit userOptionProvider: UserOptionProvider[F]): UserOptionService[F] = new UserOptionService[F] {
    override def setOption(userId: Long, key: String, value: String): F[Unit] = {
      for {
        o <- userOptionProvider.findByKeyAndUserId(key, userId)
        _ <- actionFromOption(o)(
          userOptionProvider.insert(UserOptionRaw(key, value, userId, DbIOProvider.findNow()))
        )(option => userOptionProvider.update(option, value))
      } yield ()
    }

    override def findOption(key: String, userId: Long): F[Option[UserOptionDB]] = {
      userOptionProvider.findByKeyAndUserId(key, userId)
    }
  }
}
