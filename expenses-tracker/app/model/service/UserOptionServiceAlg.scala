package model.service

import cats.effect.MonadCancelThrow
import cats.implicits.{toFlatMapOps, toFunctorOps}
import model.dao.algebr.UserOptionProvider
import model.dao.io.DbIOProvider
import model.entity.useroption.UserOptionRaw
import model.service.CommonService.actionFromOption

trait UserOptionServiceAlg[F[_]] {
  def setOption(userId: Long, key: String, value: String): F[Unit]
}

object UserOptionServiceAlg {
  def make[F[_]: MonadCancelThrow](implicit userOptionProvider: UserOptionProvider[F]): UserOptionServiceAlg[F] =
    (userId: Long, key: String, value: String) => {
      for {
        o <- userOptionProvider.findByKeyAndUserId(key, userId)
        _ <- actionFromOption(o)(
          userOptionProvider.insert(UserOptionRaw(key, value, userId, DbIOProvider.findNow()))
        )(option => userOptionProvider.update(option, value))
      } yield ()
    }
}
