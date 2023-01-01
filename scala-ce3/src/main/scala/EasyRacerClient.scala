import cats._, cats.implicits._
import cats.data.Chain

import cats.effect._, cats.effect.implicits._
import cats.effect.std.CountDownLatch

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
        case Succeeded(fa) => fiber.cancel *> fa
        case _             => fiber.join.flatMap(_.embed(
          IO.raiseError(new java.util.concurrent.CancellationException("Outcome was Canceled"))
        ))
      }
    }

    def raceSuccess(right: IO[A]): IO[A] = {
      IO.uncancelable { poll =>
        poll {
          left.racePair(right).flatMap { _ match {
            case Left((outcome, fiber))  => cascade(outcome, fiber)
            case Right((fiber, outcome)) => cascade(outcome, fiber)
          }}
        }
      }
    }
  }

  // raceSuccessAll thanks to Daniel Spiewak at <https://twitter.com/djspiewak/status/1609609781517639680>
  def raceSuccessAll[F[_], G[_], E, A](
      fas: G[F[A]])(
      implicit F: GenConcurrent[F, E],
      G: Traverse[G])
      : F[Either[Chain[E], A]] = {

    val permits = fas.size.toInt

    F uncancelable { poll =>
      for {
        complete <- CountDownLatch[F](permits)
        
        success <- F.deferred[A]
        errors <- F.ref[Chain[E]](Chain.nil)

        fibers <- fas traverse { fa =>
          val staged = fa guaranteeCase { oc =>
            complete.release *> (oc match {
              case Outcome.Succeeded(ifa) =>
                ifa.flatMap(success.complete(_)).void

              case Outcome.Errored(e) => 
                errors.update(_ :+ e)

              case Outcome.Canceled() => 
                F.unit
            })
          }

          staged.start
        }

        // wait for either the first success, or all fibers to complete
        // regardless of which case we end up in, cancel everything when we're done
        _ <- poll(F.race(complete.await, success.get).guarantee(fibers.parTraverse_(_.cancel)))
        maybeA <- success.tryGet
        result <- maybeA match {
          case Some(a) => a.asRight[Chain[E]].pure[F]
          case None => errors.get.map(_.asLeft[A])
        }
      } yield result
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

  def scenario3(client: Client[IO], scenarioUrl: Int => Uri) = {
    val url = scenarioUrl(3)
    val reqs = List.fill(10000)(client.expect[String](GET(url)))
    
    raceSuccessAll(reqs).flatMap(_.fold(_ => IO.raiseError[String](new RuntimeException("all failed")), IO.pure))
  }

  def all(client: Client[IO], scenarioUrl: Int => Uri) =
    List((scenario1 _), 
      (scenario2 _),
      (scenario3 _) /* ,
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
