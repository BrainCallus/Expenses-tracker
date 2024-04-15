package model.util

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import model.dao.io.DbIOProvider.logger
import model.dao.io.DbIOProvider
import model.entity.pays.{ExpenseRaw, ExpensesType}
import model.entity.user.UserWithPassword
import model.exception.DBException
import model.util.Ternary._
import org.apache.commons.codec.digest.DigestUtils

import java.nio.charset.StandardCharsets
import java.time.{LocalDate, Month}
import scala.util.Random

object DBUtils {
  def getPasswordEncrypted(password: String): String = encrypt(password)

  private[this] def encrypt(string: String): String = DigestUtils.sha256Hex(string.getBytes(StandardCharsets.UTF_8))

  implicit class DaoOptions[+T](io: IO[T]) {
    def safeRunToOption[K](implicit ev: T <:< Option[K]): Option[K] = {
      ev.liftCo(io)
        .redeemWith(
          e => logger.error(e)("SqlException:\n").map(_ => None),
          IO.pure
        )
        .unsafeRunSync()
    }

    def toEitherDBException(field: String = "general"): Either[DBException, T] =
      io.redeemWith(
        e =>
          logger
            .info(e)("SqlException:\n")
            .map(_ => Left(DBException(field, "Unable to provide DB operation through occurred exception during it."))),
        IO.pure(_).map(Right(_))
      ).unsafeRunSync()
  }

  // random init db for testing. Run this code after build docker to don't insert values in tables by hand
  private val categories = ExpensesType.values.toList
  private val loginName = List("test", "pupa", "lupa", "lolkek", "tester")
  private val defaultPass = "12345"

  private def initStartUsers(): Unit =
    loginName map (login => UserWithPassword(login, login, getPasswordEncrypted(defaultPass))) foreach (user => {
      DbIOProvider.userProvider.use(_.insert(user)).unsafeRunSync()
    })

  private def initExpenses(): Unit = {
    for (id <- loginName.indices) {
      for (_ <- 0 until 300) {
        val today = LocalDate.now()
        val year = today.getYear - Random.nextInt(2) // current or previous
        val yearIsCurYear: Boolean = year == today.getYear
        val month = Random.nextInt(if (year == today.getYear) today.getMonthValue else 11) + 1
        val day = Random.nextInt(
          Ternary(yearIsCurYear && month == today.getMonthValue) ?? (today.getDayOfMonth, Month.of(month).maxLength())
        ) + 1

        val sum = Random.nextInt(7000) + 100
        val expense = ExpenseRaw(
          sum,
          categories(Random.nextInt(categories.length)),
          id + 1,
          LocalDate.of(year, month, Ternary(month == 2) ?? (math.min(28, day), day))
        )
        try {
          DbIOProvider.payTypeProvider.use(_.insert[ExpenseRaw](expense)).unsafeRunSync()
        } catch {
          case e: Exception => println(e.toString)
        }
      }
    }

  }

  def main(args: Array[String]): Unit = {
    val io = for {
      _ <- IO(initStartUsers())
      _ <- IO(initExpenses())
    } yield ()
    io.redeem(e => println("Failed to init db \n" + e.toString), _ => println("success")).unsafeRunSync()
  }
}
