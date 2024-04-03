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
import scala.concurrent.duration.DurationInt
import scala.util.Try

trait HttpPyServSarimaAlg[F[_]] {
  //implicit val F: Concurrent[F]
  def run(task: => F[List[Double]]): F[Outcome[F, Throwable, List[Double]]]
  def runOnCancel(task: =>F[List[Double]]): F[Either[PyException, List[Double]]]
  def getPredictionsFromPy(days: Int, data: List[(LocalDate, Double)]): List[Double] = {
    val response = Http("http://127.0.0.1:5000/calculate")
      .postData(Json.toJson((days, data.map(_._2).toArray)).toString)
      .header("Content-Type", "application/json")
      .option(HttpOptions.readTimeout(100000))
      .asString
    val resultJson = Json.parse(response.body)
    resultJson.as[List[Double]]
  }
  def buildTask(days: Int, data: List[(LocalDate, Double)]): EitherT[F, PyException, List[Double]]

}

object HttpPyServSarimaAlg {
  implicit val logger: SelfAwareStructuredLogger[IO] = Slf4jLogger.getLogger[IO]

  def make[F[_]:MonadCancelThrow](implicit F: Concurrent[F]): HttpPyServSarimaAlg[F] = new HttpPyServSarimaAlg[F] {

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
        case Canceled() => F.pure(Left(PyException("error", "Unexpected internal server error. Can't compute forecast")))
        case Errored(e) =>
          logger.info(e)("finished with error")
          F.pure(Left(PyException("error", "Unexpected internal server error. Can't compute forecast")))
        case Succeeded(fa) => fa.map(x => Right(x))
      }
    }

    // todo: inline in run to catch ot to EitherT
    override def buildTask(days: Int, data: List[(LocalDate, Double)]): EitherT[F, PyException, List[Double]] = {
      EitherT.fromEither(Try({
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
      ))
    }

  }


 // def run[F[_]: MonadCancelThrow](task: F[List[Double]])(implicit F: Concurrent[F]): F[Outcome[F, Throwable, List[Double]]] = {
 //   //Concurrent[F].start(task).flatMap()
 //   val res = for {
 //     fiber <- Concurrent[F].start(task)
 //     _ <- F.handleError(F.pure(logger.info("fiber))")))(e => {
 //       fiber.cancel
 //       logger.error(e)("Python exception").flatMap(_ =>
 //       IO.raiseError(PyException("error", "Unexpected internal server error. Can't compute forecast")))
 //     })
 //     aftermath <- fiber.join
 //   } yield aftermath
 //   res
 // }
//
 // private def runOnCancel[F[_]: MonadCancelThrow](task: F[List[Double]])(implicit f: Concurrent[F]): F[Either[PyException, List[Double]]] =
 //   run(task).flatMap {
 //     case Canceled() => f.pure(Left(PyException("error", "Unexpected internal server error. Can't compute forecast")))
 //     case Errored(e) =>
 //       logger.info(e)("finished with error")
 //       f.pure(Left(PyException("error", "Unexpected internal server error. Can't compute forecast")))
 //     case Succeeded(fa) => fa.map(x => Right(x))
 //   }
//
//
//
//
 // private def getPredictionsFromPy(days: Int, data: List[(LocalDate, Double)]): List[Double] = {
 //   val response = Http("http://127.0.0.1:5000/calculate")
 //     .postData(Json.toJson((days, data.map(_._2).toArray)).toString)
 //     .header("Content-Type", "application/json")
 //     .option(HttpOptions.readTimeout(100000))
 //     .asString
 //   val resultJson = Json.parse(response.body)
 //   resultJson.as[List[Double]]
 // }
//
 // private def buildTask[F[_]](days: Int, data: List[(LocalDate, Double)])(implicit F: Concurrent[F]): F[List[Double]] =
 //   F.pure(
 //     getPredictionsFromPy(days, data)
 //   )
//
 // def runWithTimeOut[F[_]: MonadCancelThrow](
 //                     days: Int,
 //                     data: List[(LocalDate, Double)],
 //                     timeoutMillis: Int = 120000
 //                   )(implicit f: Concurrent[F]): F[Either[PyException, List[Double]]] = {
 //   val task = buildTask[F](days, data)
 //   runOnCancel(task)
 // }
}
