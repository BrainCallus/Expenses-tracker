package model.service

import cats.effect.IO
import model.dao.algebr._
import model.dao.io.DbIOProvider
import model.service.ForecastService.make

object IoImplicits {
  implicit val userProvider: UserProvider[IO] = UserProvider.make(DbIOProvider.xa)
  implicit val userOptionProvider: UserOptionProvider[IO] = UserOptionProvider.make(DbIOProvider.xa)
  implicit val payTypeProvider: PayTypeProvider[IO] = PayTypeProvider.make(DbIOProvider.xa)
  implicit val commonService: CommonServiceAlg[IO] = CommonServiceAlg.make[IO]()
  implicit val userService: UserService[IO] = UserService.make[IO]
  implicit val forecastService: ForecastService[IO] = ForecastService.make[IO]
}
