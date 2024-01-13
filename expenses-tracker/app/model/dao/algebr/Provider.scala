package model.dao.algebr

import cats.effect.kernel.{MonadCancelThrow, Resource}
import doobie.ConnectionIO
import doobie.hikari.HikariTransactor
import doobie.util.transactor.Transactor
import doobie.implicits._

trait Provider[F[_]] {}

object Provider {
  def transact[F[_]: MonadCancelThrow, K](xa: Transactor[F])(con: ConnectionIO[K]): F[K] = con.transact(xa)

  def makeProvider[F[_]: MonadCancelThrow, P[F[_]] <: Provider[F]](
    res: Resource[F, HikariTransactor[F]],
    makeProvider: Transactor[F] => P[F]
  ): Resource[F, P[F]] =
    res.map(xa => makeProvider(xa))

  def provideOperation[F[_]: MonadCancelThrow, T, P[F[_]] <: Provider[F]](res: Resource[F, P[F]])(
    f: P[F] => F[T]
  ): F[T] =
    res.use(prov => f(prov))
}
