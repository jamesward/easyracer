import cats._, cats.implicits._

import cats.effect._, cats.effect.implicits._

import org.http4s._, org.http4s.implicits._
import org.http4s.Method._

import org.http4s.client.Client
import org.http4s.client.dsl.io._

import org.http4s.ember.client.EmberClientBuilder
import cats.effect.kernel.Outcome.Succeeded

object EasyRacerClient extends IOApp.Simple {
  val cr = EmberClientBuilder.default[IO].build

  // Questionable semantics: throws away errors!
  implicit class RaceSuccess[A](left: IO[A]) {
    private def cascade[A](outcome: OutcomeIO[A], fiber: FiberIO[A]): IO[A] = {
      outcome match {
        case Succeeded(fa) => fa
        case _             => fiber.join.flatMap { right =>
          right.fold(
            IO.raiseError[A](new RuntimeException("cancelled")),
            IO.raiseError[A],
            identity
          )
        }
      }
    }

    def raceSuccess(right: IO[A]): IO[A] = {
      val eitherIO = left.racePair(right)

      eitherIO.flatMap { _ match {
        case Left((outcome, fiber))  => cascade(outcome, fiber)
        case Right((fiber, outcome)) => cascade(outcome, fiber)
      }}
    }
  }

  def scenario1(client: Client[IO], scenarioUrl: Int => Uri) = {
    val url = scenarioUrl(1)
    val req = client.expect[String](GET(url))
 
    req.raceSuccess(req)
  }

  def scenario2(client: Client[IO], scenarioUrl: Int => Uri) = {
    val url = scenarioUrl(2)
    val req = client.expect[String](GET(url))
 
    req.raceSuccess(req)
  }

  def all(client: Client[IO], scenarioUrl: Int => Uri) =
    List((scenario1 _), 
      (scenario2 _) /* ,
      (scenario3 _),
      (scenario4 _),
      (scenario5 _),
      (scenario6 _),
      (scenario7 _),
      (scenario8 _) */
    ).parTraverse { f =>
      f(client, scenarioUrl)
    }

  def run: IO[Unit] = {
    val nar = new RuntimeException("not all right")
    def scenarioUrl(scenario: Int) = Uri.unsafeFromString(s"http://localhost:8080/$scenario")

    cr.use { client =>
      val ios    = all(client, scenarioUrl)
      val result = ios.map { _.forall(_ == "right") }

      result.ifM(IO.unit, IO.raiseError[Unit](nar))
    }
  }
}
