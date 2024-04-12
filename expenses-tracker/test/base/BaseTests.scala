package base

import cats.effect.IO
import doobie.implicits._
import cats.effect.unsafe.implicits.global
import doobie.implicits.toSqlInterpolator
import model.dao.io.DbIOProvider.xa
import model.entity.pays._
import model.entity.user.UserWithId
import model.entity.useroption.UserOptionDB
import org.scalatest.{Assertion, Assertions}
import org.scalatest.matchers.should.Matchers
import util.account._
import util.account.TestUserAccount._
import model.service.IoImplicits._

object BaseTests extends Assertions with Matchers {

  def withNewBlankUser(tests: BlankUserAccount => Unit): Unit = {
    withNewAccount("newUser", IO.pure(createBlankAccout("newUser")).flatMap(_ => getNewUserAccountByLogin("newUser")))(
      tests
    )
  }

  def withAnotherUser(tests: AccountWithPays => Unit): Unit = {
    withNewAccount("another", IO.pure(createUserAccount("another")).flatMap(_ => getUserAccountByLogin("another")))(
      tests
    )

  }

  private def withNewAccount[T <: TestUserAccount](login: String, creation: IO[T])(tests: T => Unit): Unit =
    (for {
      u <- creation
      _ <- IO.pure(tests(u))
    } yield ())
      .guarantee(for {
        u <- getUserAccountByLogin(login)
        _ <- IO.pure(removeUserAccount(u))
      } yield ())
      .unsafeRunSync()

  /*
main idea not to use in each test class beforeAll and afterAll wrapping eachTest in try-finally to clean DB but instead
initiate this actions as a tests meanwhile checking whether that gone successfully regardless to other tests results
   */

  def createNewUser(login: String): Assertion = {
    assertUserNotExist(login)
    createUser(login).unsafeRunSync()
    assertUserExist(login)
  }

  def removeUserByLogin(login: String): Assertion = {
    dropUser(login).unsafeRunSync()
    assertUserNotExist(login)
  }

  def createBlankAccout(login: String): Assertion = {
    assertUserNotExist(login)
    TestUserAccount.createNewUser(login).unsafeRunSync()
    assertUserExist(login)

  }

  def createUserAccount(login: String): Assertion = {
    assertUserNotExist(login)
    val account = TestUserAccount.createUserAccount(login).unsafeRunSync()
    assertUserExist(login)
    assertResult(account.scheduledPays.length)(
      payTypeProvider.findByUser[ScheduledPayFull](account.id, isExpense = false).unsafeRunSync().length
    )
    assertResult(account.expenses.length)(
      payTypeProvider.findByUser[ExpenseFull](account.id, isExpense = true).unsafeRunSync().length
    )
  }

  def removeUserAccount(user: TestUserAccount): Assertion = {
    TestUserAccount.completelyDropUser(user).unsafeRunSync()
    assertUserNotExist(user.login)
    payTypeProvider.findByUser[ScheduledPayFull](user.id, isExpense = false).unsafeRunSync() shouldBe Nil
    payTypeProvider.findByUser[ExpenseFull](user.id, isExpense = true).unsafeRunSync() shouldBe Nil
    sql"""SELECT * FROM userOption WHERE userid=${user.id}"""
      .query[UserOptionDB]
      .to[List]
      .transact(xa)
      .unsafeRunSync() shouldBe Nil
  }

  def assertUserNotExist(login: String): Assertion = {
    val result = userProvider.findByLogin(login).unsafeRunSync()
    result shouldBe None
  }

  def assertUserExist(login: String): Assertion =
    assertResult(true, "Fail to createUser")(userProvider.findByLogin(login).unsafeRunSync().get.isInstanceOf[UserWithId])
}
