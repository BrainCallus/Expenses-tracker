package model.py

import cats.effect._
import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.unsafe.implicits.global
import model.exception.PyException
import play.api.libs.json._
import scalaj.http._
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.time.LocalDate
import scala.annotation.tailrec
import scala.concurrent.duration.DurationInt

object HttpPyServSarima {
  implicit val logger: SelfAwareStructuredLogger[IO] = Slf4jLogger.getLogger[IO]
  def run(task: IO[List[Double]]): IO[Outcome[IO, Throwable, List[Double]]] = {
    val res = for {
      fiber <- task.start
      _ <- IO.pure(println("fiber))")).handleErrorWith { error =>
        fiber.cancel *> logger.info(error)("Python exception") .flatMap( _=>IO.raiseError(
          PyException("error", "Unexpected internal server error. Can't compute forecast")
        ))
      }
      aftermath <- fiber.join
    } yield aftermath
    res
  }

  @tailrec
  private def runOnCancel(task: IO[List[Double]]): IO[Either[PyException, List[Double]]] =
    run(task).unsafeRunSync() match {
      case Canceled() => runOnCancel(task)
      case Errored(e) =>
        logger.info(e)("finished with error") .flatMap(_ => IO.pure(
          Left(PyException("error", "Unexpected internal server error. Can't compute forecast")))
        )
      case Succeeded(fa) => fa.map(x => Right(x))
    }

  private def getPredictionsFromPy(days: Int, data: List[(LocalDate, Double)]): List[Double] = {
    val response = Http("http://127.0.0.1:5000/calculate")
      .postData(Json.toJson((days, data.map(_._2).toArray)).toString)
      .header("Content-Type", "application/json")
      .option(HttpOptions.readTimeout(100000))
      .asString
    val resultJson = Json.parse(response.body)
    resultJson.as[List[Double]]
  }

  private def buildTask[F[_]](days: Int, data: List[(LocalDate, Double)])(implicit F: Sync[F]): F[List[Double]] =
    F.delay(
      getPredictionsFromPy(days, data)
    )

  def runWithTimeOut(
    days: Int,
    data: List[(LocalDate, Double)],
    timeoutMillis: Int = 120000
  ): IO[Either[PyException, List[Double]]] = {
    val task = buildTask[IO](days, data)
    runOnCancel(task).timeoutTo(
      timeoutMillis.millis,
      logger.error("Timelimit exceed") *>
        IO.pure(Left(PyException("error", s"Server timeout ${timeoutMillis / 1000}s exceed. Please, try again later")))
    )
  }
}
