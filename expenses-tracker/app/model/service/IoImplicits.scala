package model.service

import cats.effect.IO
import model.dao.algebr._
import model.dao.io.DbIOProvider

object IoImplicits {
  implicit val userProvider: UserProvider[IO] = UserProvider.make(DbIOProvider.xa)
  implicit val userOptionProvider: UserOptionProvider[IO] = UserOptionProvider.make(DbIOProvider.xa)
  implicit val payTypeProvider: PayTypeProvider[IO] = PayTypeProvider.make(DbIOProvider.xa)
  implicit val scheduledPayProvider : ScheduledPayProvider[IO] = ScheduledPayProvider.make(DbIOProvider.xa)

  implicit val commonService: CommonService[IO] = CommonService.make[IO]()
  implicit val userOptionService : UserOptionService[IO] = UserOptionService.make[IO]
  implicit val userService: UserService[IO] = UserService.make[IO]
  implicit val forecastService: ForecastService[IO] = ForecastService.make[IO]
  implicit val expenseService: ExpenseService[IO] = ExpenseService.make[IO]
}
