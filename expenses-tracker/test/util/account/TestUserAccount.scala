package util.account

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.implicits.toFunctorOps
import doobie.implicits._
import model.dao.io.DbIOProvider.{findNow, xa}
import model.entity.pays.{ExpenseFull, ScheduledPayFull}
import model.entity.user.UserWithPassword
import model.service.IoImplicits._
import model.util.DBUtils.getPasswordEncrypted
import util.CommonTestUtils.defaultPassword
import util.option.TestUserOption.dropUserOptions
import util.pays.TestPays._

import java.sql.SQLException

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
      u <- userProvider.findByLogin(login).map {
        case Some(u) => u
        case None => throw new SQLException("Just created user not found")
      }
    } yield BlankUserAccount(u.id, u.login, u.name)

  def createUserAccount(login: String): IO[AccountWithPays] = {
    for {
      u <- createNewUser(login)
      account <- {
        for {
          _ <- createUserExpenses(u.id, 200)
          _ <- createUserFollowScheduledPays(u.id, 20)
          _ <- userOptionService.setOption(u.id, "lastTimeUpdated", findNow().toString)
          acc <- getUserAccountByLogin(login)
        } yield acc
      }
    } yield account
  }

  def getUserAccountByLogin(login: String): IO[AccountWithPays] = {
    for {
      u <- getNewUserAccountByLogin(login)
      expenses <- payTypeProvider.findByUser[ExpenseFull](u.id, isExpense = true)
      pays <- payTypeProvider.findByUser[ScheduledPayFull](u.id, isExpense = false)
    } yield AccountWithPays(u, expenses, pays)
  }

  def createUser(userLogin: String): IO[Unit] =
    userProvider.insert(UserWithPassword(userLogin, userLogin, getPasswordEncrypted(defaultPassword)))

  def dropUser(userLogin: String): IO[Unit] = {
    sql"""DELETE FROM "user" WHERE login=$userLogin""".update.run.void.transact(xa)
  }

  def completelyDropUser(userAccount: TestUserAccount): IO[Unit] = {
    for {
      _ <- dropUserExpenses(userAccount)
      _ <- dropUserScheduledPays(userAccount)
      _ <- dropUserOptions(userAccount.id)
      _ <- dropUser(userAccount.login)
    } yield ()
  }
}
