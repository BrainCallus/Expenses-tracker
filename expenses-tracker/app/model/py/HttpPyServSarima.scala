package model.py

import cats.data.EitherT
import cats.effect._
import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.implicits.{toFlatMapOps, toFunctorOps}
import model.exception.PyException
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import play.api.libs.json._
import scalaj.http._

import java.time.LocalDate
import scala.util.Try

trait HttpPyServSarima[F[_]] {
  def run(task: => F[List[Double]]): F[Outcome[F, Throwable, List[Double]]]
  def runOnCancel(task: => F[List[Double]]): F[Either[PyException, List[Double]]]
  def buildTask(days: Int, data: List[(LocalDate, Double)]): EitherT[F, PyException, List[Double]]
}

object HttpPyServSarima {
  implicit val logger: SelfAwareStructuredLogger[IO] = Slf4jLogger.getLogger[IO]

  def make[F[_]: MonadCancelThrow](implicit F: Concurrent[F]): HttpPyServSarima[F] = new HttpPyServSarima[F] {

    override def run(task: => F[List[Double]]): F[Outcome[F, Throwable, List[Double]]] = {
      val res = for {

        _ <- F.handleError(F.pure(logger.info("fiber))")))(e => {
//
          logger.error(e)("Python exception")
        })
        fiber <- Concurrent[F].start(task)
        _ <- F.handleError(F.pure(logger.info("fiber))")))(e => {
          fiber.cancel
          //
          logger.error(e)("Python exception")
        })
        aftermath <- fiber.join
      } yield aftermath
      res
    }

    override def runOnCancel(task: => F[List[Double]]): F[Either[PyException, List[Double]]] = {
      run(task).flatMap {
        case Canceled() =>
          F.pure(Left(PyException("error", "Unexpected internal server error. Can't compute forecast")))
        case Errored(e) =>
          logger.info(e)("finished with error")
          F.pure(Left(PyException("error", "Unexpected internal server error. Can't compute forecast")))
        case Succeeded(fa) => fa.map(x => Right(x))
      }
    }

    override def buildTask(days: Int, data: List[(LocalDate, Double)]): EitherT[F, PyException, List[Double]] = {
      EitherT.fromEither(
        Try({
          val response = Http("http://127.0.0.1:5000/calculate")
            .postData(Json.toJson((days, data.map(_._2).toArray)).toString)
            .header("Content-Type", "application/json")
            .option(HttpOptions.readTimeout(100000))
            .asString
          Json.parse(response.body)
        }).fold(
          e => {
            logger.error(e)("Json parse failed")
            Left(PyException("error", "Unexpected internal server error. Can't compute forecast"))
          },
          result => Right(result.as[List[Double]])
        )
      )
    }
  }
}
