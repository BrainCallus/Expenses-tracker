package controllers

import cats.effect.unsafe.implicits.global
import util.CommonTestUtils._
import model.util.DateUtil.{DateCalc, getNow}
import play.api.libs.json.{JsValue, Json}
import org.scalatestplus.play._
import org.scalatestplus.play.guice._
import play.api.Play.materializer
import play.api.test.CSRFTokenHelper._
import play.api.test.Helpers._
import play.api.test._
import org.jsoup.Jsoup
import base.BaseTests
import cats.effect.IO
import doobie.{Read, Update}
import model.dao.algebr.PayTypeProvider
import model.dao.io.DbIOProvider.xa
import play.api.mvc.{Action, AnyContent}
import doobie.implicits._
import model.entity.pays._
import model.entity.useroption.UserOptionRaw
import util.account.TestUserAccount._
import util.account._
import util.option.TestUserOption.{dropUserOptions, getLastTimeUpdated}
import util.pays.TestPays.paysInSegment
import model.service.IoImplicits._

import java.time.{LocalDateTime, ZoneOffset}

class MyExpensesControllerSpec extends PlaySpec with GuiceOneAppPerTest with Injecting {

  private val login = "testUser"

  "MyExpensesController tests" should {
    BaseTests.createUserAccount(login)
    allTests().unsafeRunSync()

    "MyExpensesController finalizeTest" should {
      "remove previously created user if it exist" in {
        base.BaseTests.removeUserAccount(getUserAccountByLogin(login).unsafeRunSync())
      }
    }
  }

  def allTests(): IO[Unit] =
    for {
      _ <- IO.pure(viewTest())
      _ <- IO.pure(
        runAddingTests[ExpenseFull](
          new MyExpensesController(stubControllerComponents()),
          getUserAccountByLogin(login).unsafeRunSync(),
          c => c.addExpense(),
          "/addExpense"
        )
      )
      _ <- IO.pure(
        runAddingTests[ScheduledPayFull](
          new MyExpensesController(stubControllerComponents()),
          getUserAccountByLogin(login).unsafeRunSync(),
          c => c.addSchedulePay(),
          "/addSheduledPay"
        )
      )
      _ <- IO.pure(
        runDeleteExpenseTests(
          new MyExpensesController(stubControllerComponents()),
          getUserAccountByLogin(login).unsafeRunSync()
        )
      )
      _ <- IO.pure(
        runChangePayStatusTests(
          new MyExpensesController(stubControllerComponents()),
          getUserAccountByLogin(login).unsafeRunSync()
        )
      )
      _ <- IO.pure(
        runGetForecastTests(
          new MyExpensesController(stubControllerComponents()),
          getUserAccountByLogin(login).unsafeRunSync()
        )
      )
    } yield ()

  def viewTest(): Unit = {
    "MyExpensesController view" should {
      "redirect to login if no registered user" in {
        val controller = new MyExpensesController(stubControllerComponents())
        val result = controller.view().apply(FakeRequest(GET, "/my-expenses").withCSRFToken)
        status(result) mustBe SEE_OTHER
        flash(result).get("errorMessage") mustBe Some("You're not logged in")
      }

      "show user expenses for past 3 month if user authenticated" in {
        val account = getUserAccountByLogin(login).unsafeRunSync()
        val controller = new MyExpensesController(stubControllerComponents())
        val myExpensesPage = controller
          .view()
          .apply(FakeRequest(GET, "/my-expenses").withSession("userId" -> account.id.toString).withCSRFToken)
        status(myExpensesPage) mustBe OK
        val contentString = contentAsString(myExpensesPage)
        contentString must include("<table class=\"grid expenses-table\" id=\"expenses\"")
        Jsoup.parse(contentString).select("td.expense-info").size() mustEqual paysInSegment(
          account.expenses,
          3.monthLater(getToday),
          getToday
        ).length
      }

      "show only text that user haven't expenses if them length 0" in {
        val blank = (for {
          _ <- IO.pure(BaseTests.createBlankAccout("blank"))
          u <- TestUserAccount.getNewUserAccountByLogin("blank")
        } yield u).unsafeRunSync()
        val controller = new MyExpensesController(stubControllerComponents())
        val myExpensesPage = controller
          .view()
          .apply(FakeRequest(GET, "/my-expenses").withSession("userId" -> blank.id.toString).withCSRFToken)
        status(myExpensesPage) mustBe OK
        val contentString = contentAsString(myExpensesPage)
        contentString must include("Nothing here yet")
        contentString mustNot include("<table class=\"grid\" id=\"expenses\"")
        TestUserAccount.dropUser("blank").unsafeRunSync()
      }

      "suggest to confirm past term scheduled pays if they exist" in {
        val account = getUserAccountByLogin(login).unsafeRunSync()
        val pastScheduled = account.getPastTermPays()
        val controller = new MyExpensesController(stubControllerComponents())
        val myExpensesPage = controller
          .view()
          .apply(FakeRequest(GET, "/my-expenses").withSession("userId" -> account.id.toString).withCSRFToken)
        status(myExpensesPage) mustBe OK
        val contentString = contentAsString(myExpensesPage)
        Jsoup.parse(contentString).select("form.confirmOrCancelScheduled").size() mustEqual pastScheduled.length
      }

    }
  }

  /** Really slow tests. Recommended to launch in last order
    */
  def runGetForecastTests(controller: MyExpensesController, account: AccountWithPays): Unit = {
    def forceSetOption(option: UserOptionRaw, interval: String) = {
      Update[UserOptionRaw](
        s"INSERT INTO \"useroption\" (\"key\", \"value\", \"userid\",\"updationtime\" ,\"creationtime\") VALUES (?,?,?,CURRENT_TIMESTAMP - INTERVAL '$interval days',CURRENT_TIMESTAMP - INTERVAL '2 days')"
      )
        .updateMany(List(option))
        .transact(xa)
        .unsafeRunSync()
    }
    "MyExpensesController monthPredict" should {
      "take value form option if last forecast value was taken later then yesterday and last update" in {
        userOptionService.setOption(account.id, getLastTimeUpdated(account.id).key, getNow.toString)
          //.flatMap(_ => IO.sleep(2.second))
          .flatMap(_ => userOptionProvider.insert(UserOptionRaw("forecast", "30000", account.id, getNow)))
          .unsafeRunSync()

        val lastUpdated = getLastTimeUpdated(account.id)
        val forecastOpt = userOptionService.findOption("forecast", account.id).unsafeRunSync().get
        forecastOpt.updationTime.compareTo(lastUpdated.updationTime) mustBe 1
        val forecast = controller.sendForecastRequest(account.id)
        status(forecast) mustBe OK
        contentAsString(forecast) must include("{\"success\":1,\"predicted\":30000}")

        val newForecastOpt = userOptionService.findOption("forecast", account.id).unsafeRunSync().get
        newForecastOpt.updationTime.compareTo(forecastOpt.updationTime) mustBe 0
      }

      "request forecast from server if lastForecast was not today even if update was earlier" in {
        dropUserOptions(account.id).unsafeRunSync()
        val yesterday = getYesterday
        val timestamp = LocalDateTime.of(yesterday.getYear, yesterday.getMonth, yesterday.getDayOfMonth, 1, 1, 1)
        forceSetOption(UserOptionRaw("forecast", timestamp.toString, account.id, timestamp), "1")

        val day2Before = 2.daysBefore(getToday)
        val lastUpdateTime =
          LocalDateTime.of(day2Before.getYear, day2Before.getMonth, day2Before.getDayOfMonth, 1, 1, 1)
        forceSetOption(UserOptionRaw("lastTimeUpdated", lastUpdateTime.toString, account.id, lastUpdateTime), "2")

        val lastUpdated = getLastTimeUpdated(account.id)
        val forecastOpt = userOptionService.findOption("forecast", account.id).unsafeRunSync().get
        lastUpdated.updationTime.compareTo(forecastOpt.updationTime) mustBe -1
        val startTime = getNow
        val forecast = controller.sendForecastRequest(account.id)
        // accelerated ))) but still much more slowly than cached
        assert(getNow.toEpochSecond(ZoneOffset.UTC) - startTime.toEpochSecond(ZoneOffset.UTC) > 10)
        status(forecast) mustBe OK
        val newForecastOpt = userOptionService.findOption("forecast", account.id).unsafeRunSync().get
        newForecastOpt.updationTime.compareTo(forecastOpt.updationTime) mustBe 1
      }

      "request forecast from server if forecast older then lastUpdateTime" in {
        userOptionService.setOption(account.id, "lastTimeUpdated", getNow.toString).unsafeRunSync()
        val lastTimeUpdatedOt = getLastTimeUpdated(account.id)
        val forecastOpt = userOptionService.findOption("forecast", account.id).unsafeRunSync().get
        lastTimeUpdatedOt.updationTime.compareTo(forecastOpt.updationTime) mustBe 1
        val startTime = getNow
        val forecast = controller.sendForecastRequest(account.id)
        assert(getNow.toEpochSecond(ZoneOffset.UTC) - startTime.toEpochSecond(ZoneOffset.UTC) > 10)
        status(forecast) mustBe OK
        val newForecastOpt = userOptionService.findOption("forecast", account.id).unsafeRunSync().get
        newForecastOpt.updationTime.compareTo(forecastOpt.updationTime) mustBe 1
      }

      "should return error if user have no lastTimeUpdated option" in {
        BaseTests.withNewBlankUser(u => {
          val forecast = controller.sendForecastRequest(u.id)
          status(forecast) mustBe OK
          contentAsString(forecast) must include(
            "{\"success\":\"\",\"error\":\"Unexpected internal server error. Can't compute forecast\"}"
          )
        })
      }
      "should return error if user have no expenses cause server can't compute predict for this case" in {
        BaseTests.withNewBlankUser(u => {
          userOptionService.setOption(u.id, "lastTimeUpdated", getNow.toString)
          val forecast = controller.sendForecastRequest(u.id)
          status(forecast) mustBe OK
          contentAsString(forecast) must include(
            "{\"success\":\"\",\"error\":\"Unexpected internal server error. Can't compute forecast\"}"
          )
        })
      }
    }
  }

  def runChangePayStatusTests(controller: MyExpensesController, account: AccountWithPays): Unit = {
    "MyExpenseController changeScheduledStatus" should {
      "change status for valid pay" in {
        val pays = account.getPastTermPays()
        val toConfirm = pays.head
        val toCancel = pays.tail.head
        val lastTimeUpdatedOption = getLastTimeUpdated(account.id)
        val change = controller.sendChangeStatusForm(toCancel.id, account.id, ScheduledPayStatus.CANCELED.toString)
        status(change) mustBe OK
        val newLastUpdated = getLastTimeUpdated(account.id)
        newLastUpdated.updationTime.compareTo(lastTimeUpdatedOption.updationTime) mustBe 1
        payTypeProvider
          .find[ScheduledPayFull](toCancel.id, isExpense = false)
          .map(_.get)
          .unsafeRunSync()
          .status mustBe ScheduledPayStatus.CANCELED

        val initSum = payTypeProvider
          .findByUser[ExpenseFull](account.id, isExpense = true)
          .unsafeRunSync()
          .map(_.sum)
          .sum
        val change2 = controller.sendChangeStatusForm(toConfirm.id, account.id, ScheduledPayStatus.FULFILLED.toString)
        status(change2) mustBe OK
        val updated2 = getLastTimeUpdated(account.id)
        updated2.updationTime.compareTo(newLastUpdated.updationTime) mustBe 1
        payTypeProvider
          .find[ScheduledPayFull](toConfirm.id, isExpense = false)
          .map(_.get)
          .unsafeRunSync()
          .status mustBe ScheduledPayStatus.FULFILLED
        payTypeProvider
          .findByUser[ExpenseFull](account.id, isExpense = true)
          .unsafeRunSync()
          .map(_.sum)
          .sum - initSum mustBe toConfirm.sum
      }

      "return error if specified pay not found" in {
        val pay = account.getFollowPays().head
        payTypeProvider.delete[ScheduledPayFull](pay).unsafeRunSync()
        payTypeProvider.find[ScheduledPayFull](pay.id, isExpense = false).unsafeRunSync() mustBe None
        val change = controller.sendChangeStatusForm(pay.id, account.id, ScheduledPayStatus.CANCELED.toString)
        status(change) mustBe OK
        contentAsString(change) must include("{\"success\":\"\",\"error\":\"Scheduled pay not found\"}")
      }

      "return error if specified pay belongs to another user" in {
        BaseTests.withAnotherUser(anotherUser => {
          val anotherPay = anotherUser.getFollowPays().head
          val change = controller.sendChangeStatusForm(anotherPay.id, account.id, ScheduledPayStatus.FULFILLED.toString)
          status(change) mustBe OK
          contentAsString(change) must include("{\"success\":\"\",\"error\":\"Scheduled pay not found\"}")
        })
      }

      "return error if performed status is invalid" in {
        val pay = account.getPastTermPays().head
        List("status", "decline", "assert", "ok", "CONFIRM", "REJECT") map { st =>
          val change = controller.sendChangeStatusForm(pay.id, account.id, st)
          status(change) mustBe OK
          contentAsString(change) must include(
            "{\"success\":\"\",\"error\":\"Invalid value: confirm or decline expected\"}"
          )
        }
      }
    }
  }

  def runDeleteExpenseTests(controller: MyExpensesController, account: AccountWithPays): Unit = {
    val firstId = account.expenses.head
    "MyExpensesController deleteExpense" should {
      "delete chosen expense" in {
        val lastTimeUpdated = getLastTimeUpdated(account.id)
        val delete = controller.sendDeleteForm(firstId.id, account.id)
        status(delete) mustBe OK
        payTypeProvider.find[ExpenseFull](firstId.id, isExpense = true).unsafeRunSync() mustBe None
        val newOption = getLastTimeUpdated(account.id)
        newOption.updationTime.compareTo(lastTimeUpdated.updationTime) mustBe 1
      }

      "just reload page if specified pay not found" in {
        val lastTimeUpdated = userOptionService.findOption("lastTimeUpdated", account.id).unsafeRunSync()
        val delete = controller.sendDeleteForm(firstId.id, account.id)
        status(delete) mustBe OK
        contentAsString(delete) must include("{\"success\":true}")
        payTypeProvider.find[ExpenseFull](firstId.id, isExpense = true).unsafeRunSync() mustBe None
        val newOption = userOptionService.findOption("lastTimeUpdated", account.id).unsafeRunSync()
        newOption.get.updationTime.compareTo(lastTimeUpdated.get.updationTime) mustBe 0
      }

      "return error if specifiedPay does not belong to user and in no way involve in DB state" in {
        BaseTests.withAnotherUser(anotherUser => {
          val anotherFirst = anotherUser.expenses.head
          val accOpt = getLastTimeUpdated(account.id)
          val anotherOpt = getLastTimeUpdated(anotherUser.id)
          val delete = controller.sendDeleteForm(anotherFirst.id, account.id)
          status(delete) mustBe OK
          contentAsString(delete) must include(
            "{\"success\":\"\",\"error\":\"You have no access\"}"
          )
          val newOpt = getLastTimeUpdated(account.id)
          newOpt.updationTime.compareTo(accOpt.updationTime) mustBe 0
          val newAnotherOpt = getLastTimeUpdated(account.id)
          assert(newAnotherOpt.updationTime.compareTo(anotherOpt.updationTime) <= 0)
          payTypeProvider.find[ExpenseFull](anotherFirst.id, isExpense = true).map(_.get).unsafeRunSync() mustBe anotherFirst
        })
      }
    }
  }

  def runAddingTests[T <: PayType: Read](
    controller: MyExpensesController,
    account: AccountWithPays,
    action: MyExpensesController => Action[AnyContent],
    route: String
  )(implicit ev: PayTypeProvider.FullExpenseOrPayEvidence[T]): Unit = {
    val isExpense = route == "/addExpense"

    def get3Days: String =
      (if (isExpense) 3.daysBefore(getToday) else 3.daysLater(getToday)).toString
    s"myExpensesController $route" should {
      "add new expense for user for correct form data" in {

        ExpensesType.values map { eType =>
          val lastTimeUpdatedOption = userOptionService.findOption("lastTimeUpdated", account.id).unsafeRunSync()
          val add = controller.sendPayForm(action, route)("1000", eType.toString, account.id.toString, get3Days)
          status(add) mustBe OK
          userOptionService.findOption("lastTimeUpdated", account.id).unsafeRunSync()
            .map(_.updationTime)
            .get
            .compareTo(lastTimeUpdatedOption.get.updationTime) mustBe 1
        }
        val newSum =
          payTypeProvider.findByUser[T](account.id, isExpense = isExpense).map(_.map(_.sum).sum).unsafeRunSync()
        assertResult(1000 * ExpensesType.values.length)(
          newSum - (if (isExpense) account.expenses else account.scheduledPays).map(_.sum).sum
        )
      }

      "print error if some fields are empty" in {

        val invalidSum = controller.sendPayForm(action, route)("", "food", account.id.toString, get3Days)
        status(invalidSum) mustBe OK
        contentAsString(invalidSum) must include(
          "{\"success\":\"\",\"errorField\":\"sum\",\"errorMessage\":\"Value is None\"}"
        )

        val invalidType = controller.sendPayForm(action, route)("1000", "invType", account.id.toString, get3Days)

        status(invalidType) mustBe OK
        contentAsString(invalidType) must include(
          "{\"success\":\"\",\"errorField\":\"expenseType\",\"errorMessage\":\"Value is None\"}"
        )

        val invalidDate =
          controller.sendPayForm(action, route)("1000", ExpensesType.FOOD.toString, account.id.toString, "")
        status(invalidDate) mustBe OK
        contentAsString(invalidDate) must include(
          "{\"success\":\"\",\"errorField\":\"date\",\"errorMessage\":\"Field shouldn't be empty\"}"
        )

        val newSum =
          payTypeProvider.findByUser[T](account.id, isExpense = isExpense).map(_.map(_.sum).sum).unsafeRunSync()
        // follows from prev test
        assertResult(1000 * ExpensesType.values.length)(
          newSum - (if (isExpense) account.expenses else account.scheduledPays).map(_.sum).sum
        )
      }

      "print error if sum is not positive" in {
        val notPositiveSum =
          controller.sendPayForm(action, route)("0", ExpensesType.FOOD.toString, account.id.toString, get3Days)
        status(notPositiveSum) mustBe OK
        contentAsString(notPositiveSum) must include(
          "{\"success\":\"\",\"errorField\":\"sum\",\"errorMessage\":\"Value should be in [1.0; 1.0E9]\"}"
        )

        val negativeSum =
          controller.sendPayForm(action, route)("-1", ExpensesType.FOOD.toString, account.id.toString, get3Days)

        status(negativeSum) mustBe OK
        contentAsString(negativeSum) must include(
          "{\"success\":\"\",\"errorField\":\"sum\",\"errorMessage\":\"Value should be in [1.0; 1.0E9]\"}"
        )
      }

      "print error if sum is not double" in {
        List("lasjkdjk", "100a", "a345", "100,4") map { st =>
          val invalidSum =
            controller.sendPayForm(action, route)(st, ExpensesType.FOOD.toString, account.id.toString, get3Days)
          status(invalidSum) mustBe OK
          contentAsString(invalidSum) must include(
            "{\"success\":\"\",\"errorField\":\"sum\",\"errorMessage\":\"Value is None\"}"
          )
        }
      }
      "print error for invalid ExpenseType" in {
        List("notFood", "lolkek", "123456", "Clozes") map { st =>
          val invalidSum = controller.sendPayForm(action, route)("1000", st, account.id.toString, get3Days)
          status(invalidSum) mustBe OK
          contentAsString(invalidSum) must include(
            "{\"success\":\"\",\"errorField\":\"expenseType\",\"errorMessage\":\"Value is None\"}"
          )
        }
      }

      s"print error for ${if (isExpense) "following" else "previous"} date" in {
        val invalidSum = controller.sendPayForm(action, route)(
          "1000",
          ExpensesType.FOOD.toString,
          account.id.toString,
          (if (isExpense) getTomorrow else getToday).toString
        )
        status(invalidSum) mustBe OK
        contentAsString(invalidSum) must include(
          "{\"success\":\"\",\"errorField\":\"date\",\"errorMessage\":" +
            s"\"You can select only ${if (isExpense) "previous" else "following"} dates\"}"
        )
      }
    }
  }

  implicit class MyExpensesControllerJsonRequest(controller: MyExpensesController) {

    // !!! DO NOT ADJUST TYPE HERE
    private def jsonRequest(f: MyExpensesController => Action[AnyContent], route: String, js: JsValue) = {
      val request = FakeRequest(POST, route)
        .withJsonBody(js)
        .withHeaders("Content-Type" -> "application/json")
        .withCSRFToken
      f(controller).apply(request)
    }

    // !!! DO NOT ADJUST TYPE HERE
    def sendPayForm(
      f: MyExpensesController => Action[AnyContent],
      route: String
    )(sum: String, expenseType: String, userId: String, date: String) =
      jsonRequest(f, route, Json.toJson(Array(userId, sum, expenseType, date)))

    def sendDeleteForm(expenseId: Long, userId: Long) =
      jsonRequest(c => c.deleteExpense(), "/deleteExpense", Json.toJson(Array(expenseId, userId)))

    def sendForecastRequest(userId: Long) =
      jsonRequest(c => c.monthPredict(), "/monthPredict", Json.toJson(userId))

    def sendChangeStatusForm(payId: Long, userId: Long, status: String) =
      jsonRequest(
        c => c.changeScheduledStatus(),
        "/changeShecduledStatus",
        Json.toJson(Array(Json.toJson(userId), Json.toJson(payId), Json.toJson(status)))
      )
  }
}
