package model.dao.io

import cats.effect._
import doobie._
import doobie.hikari.HikariTransactor
import doobie.implicits._
import model.util.DBUtils.DaoOptions
import model.dao.algebr._
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.sql.Timestamp
import java.time.LocalDateTime

object DbIOProvider {
  implicit val logger: SelfAwareStructuredLogger[IO] = Slf4jLogger.getLogger[IO]

  val xa: Transactor[IO] = Transactor.fromDriverManager[IO](
    "org.postgresql.Driver",
    "jdbc:postgresql:expenses",
    "root", // username
    "root" // password
  )

  private val postgres: Resource[IO, HikariTransactor[IO]] = for {
    ce <- ExecutionContexts.fixedThreadPool[IO](32)
    xa <- HikariTransactor.newHikariTransactor[IO](
      "org.postgresql.Driver",
      "jdbc:postgresql:expenses",
      "root",
      "root",
      ce
    )
  } yield xa

  private val userProvider: Resource[IO, UserProvider[IO]] = Provider.makeProvider(postgres, UserProvider.make[IO])
   val payProvider: Resource[IO, PayTypeProvider[IO]] = Provider.makeProvider(postgres, PayTypeProvider.make[IO])
  private val scheduledPayProvider: Resource[IO, ScheduledPayProvider[IO]] =
    Provider.makeProvider(postgres, ScheduledPayProvider.make[IO])
  val userOptionProvider: Resource[IO, UserOptionProvider[IO]] =
    Provider.makeProvider(postgres, UserOptionProvider.make[IO])

  def userOperation[T](f: UserProvider[IO] => IO[T]): IO[T] =
    Provider.provideOperation(userProvider)(f)

  def payTypeOperation[T](f: PayTypeProvider[IO] => IO[T]): IO[T] =
    Provider.provideOperation(payProvider)(f)

  def userOptionOperation[T](f: UserOptionProvider[IO] => IO[T]): IO[T] =
    Provider.provideOperation(userOptionProvider)(f)

  def scheduledPayOperation[T](f: ScheduledPayProvider[IO] => IO[T]): IO[T] =
    Provider.provideOperation(scheduledPayProvider)(f)

  def findNow(): LocalDateTime = {
    sql"""SELECT NOW()""".query[String].option.transact(xa).safeRunToOption match {
      case None =>
        logger.error("Fail something completely wrong! Cannot get date")
        LocalDateTime.now()
      case Some(value) => Timestamp.valueOf(value.substring(0, value.length - 3)).toLocalDateTime
    }
  }
}
