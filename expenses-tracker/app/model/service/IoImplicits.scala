package model.service

import cats.effect.IO
import model.dao.io.DbIOProvider._

object IoImplicits {
  implicit val commonService: CommonService[IO] = CommonService.make[IO]()
  implicit val userOptionService: UserOptionService[IO] = UserOptionService.make[IO]
  implicit val userService: UserService[IO] = UserService.make[IO]
  implicit val expenseService: ExpenseService[IO] = ExpenseService.make[IO]
  implicit val forecastService: ForecastService[IO] = ForecastService.make[IO]
}
