package util.account

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.implicits.toFunctorOps
import util.CommonTestUtils.defaultPassword
import doobie.implicits._
import model.util.DBUtils.getPasswordEncrypted
import model.dao.io.DbIOProvider.{findNow, xa}
import model.dao.io.{ExpenseDao, UserDao}
import model.entity.pays.{ExpenseFull, ScheduledPayFull}
import model.entity.user.UserWithPassword
import model.service.UserOptionService
import util.option.TestUserOption.dropUserOptions
import util.pays.TestPays._

trait TestUserAccount {
  def id: Long

  def login: String
}

object TestUserAccount {
  def createNewUser(login: String): IO[BlankUserAccount] = {
    createUser(login).unsafeRunSync()
    getNewUserAccountByLogin(login)
  }

  def getNewUserAccountByLogin(login: String): IO[BlankUserAccount] =
    for {
      u <- UserDao.findByLogin(login).map(_.get)
    } yield BlankUserAccount(u.id, u.login, u.name)

  def createUserAccount(login: String): IO[AccountWithPays] = {
    for {
      u <- createNewUser(login)
      account <- createUserExpenses(u.id, 200)
        .flatMap(_ => createPastUserScheduledPays(u.id, 20))
        .flatMap(_ => createUserFollowScheduledPays(u.id, 50))
        .flatMap(_ => UserOptionService.setOption(u.id, "lastTimeUpdated", findNow().toString))
        .flatMap(_ => getUserAccountByLogin(login))
    } yield account
  }

  def getUserAccountByLogin(login: String): IO[AccountWithPays] = {
    for {
      u <- getNewUserAccountByLogin(login)
      expenses <- ExpenseDao.findAllForUser[ExpenseFull](u.id, issExpense = true)
      pays <- ExpenseDao.findAllForUser[ScheduledPayFull](u.id, issExpense = false)
    } yield AccountWithPays(u, expenses, pays)
  }

  def createUser(userLogin: String): IO[Unit] =
    UserDao.insert(UserWithPassword(userLogin, userLogin, getPasswordEncrypted(defaultPassword)))

  def dropUser(userLogin: String): IO[Unit] = {
    sql"""DELETE FROM "user" WHERE login=$userLogin""".update.run.void.transact(xa)
  }

  def completelyDropUser(userAccount: TestUserAccount): IO[Unit] = {
    dropUserExpenses(userAccount)
      .flatMap(_ => dropUserScheduledPays(userAccount))
      .flatMap(_ => dropUserOptions(userAccount.id))
      .flatMap(_ => dropUser(userAccount.login))
  }

}
