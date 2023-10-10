import scala.concurrent.duration._

import cats._, cats.implicits._
import cats.data.Chain

import cats.effect._, cats.effect.implicits._
import cats.effect.std.CountDownLatch

import org.http4s._
import org.http4s.Method._

import org.http4s.client.Client
import org.http4s.client.dsl.io._

import org.http4s.ember.client.EmberClientBuilder


import cats.effect.kernel.Outcome.Succeeded

object EasyRacerClient extends IOApp.Simple {
  val cr = EmberClientBuilder.default[IO]
    .withMaxTotal(12000)
    .build

  // Questionable semantics: throws away errors!
  implicit class RaceSuccess[A](left: IO[A]) {
    private def cascade(outcome: OutcomeIO[A], fiber: FiberIO[A]): IO[A] = {
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
                ifa.flatMap(success.complete).void

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

  // Fabio Labella's multiRace, at <https://gitter.im/typelevel/cats-effect?at=5f479a3e89cf2d584b7c65cf>
  def multiRace[F[_]: Concurrent, A](fas: List[F[A]]): F[A] = {
    def spawn[B](fa: F[B]): Resource[F, Unit] =
      Resource.make(fa.start)(_.cancel).void

    def finish(fa: F[A], d: Deferred[F, Either[Throwable, A]]): F[Unit] =
      fa.attempt.flatMap(d.complete).void

    Deferred[F, Either[Throwable, A]]
      .flatMap { result =>
        fas
          .traverse(fa => spawn(finish(fa, result)))
          .use(_ => result.get.rethrow)
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

    multiRace(reqs)
  }

  def scenario4(client: Client[IO], scenarioUrl: Int => Uri) = {
    val url = scenarioUrl(4)
    val req = client.expect[String](GET(url))

    req.timeout(1.seconds).raceSuccess(req)
  }

  def scenario5(client: Client[IO], scenarioUrl: Int => Uri) = {
    val url = scenarioUrl(5)
    // This captures the "require success and get theh body as a String" constraint
    val req = client.expect[String](GET(url))

    req.raceSuccess(req)
  }

  def scenario6(client: Client[IO], scenarioUrl: Int => Uri) = {
    val url = scenarioUrl(6)
    // This captures the "require success and get theh body as a String" constraint
    val req = client.expect[String](GET(url))

   /* multiRace(List(req, req, req)) fails here, because multiRace races to completion,
    * and the expectation is that we race to the first SUCCESSFUL completion. Daniel
    * Spiewak's raceSuccessAll captures this, whereas Fabio Labella' multiRace captures
    * the race-to-first-completion case.
    */
    raceSuccessAll(List(req, req, req)).flatMap(_.fold(
      c => IO.raiseError[String](new RuntimeException(c.toString)),
      IO.pure
    ))
  }

  def scenario7(client: Client[IO], scenarioUrl: Int => Uri) = {
    val url = scenarioUrl(7)
    val req = client.expect[String](GET(url))

    // todo: sometimes the first req can take a second or 2 to start which can break the hedge check which verifies the second request starts 2 seconds after the first one
    //   but it isn't clear how to resolve that as there isn't a way to know when the req is connected, then send the second one
    req.raceSuccess(IO.sleep(4.seconds) *> req)
  }

  def scenario8(client: Client[IO], scenarioUrl: Int => Uri) = {
    def req(client: Client[IO], url: Uri) = client.expect[String](GET(url))

    val open = req(client, scenarioUrl(8) +? "open")
    def use(id: String) = req(client, scenarioUrl(8) +? ("use" -> id))
    def close(id: String) = req(client, scenarioUrl(8) +? ("close" -> id))

    val reqRes = Resource.make(open)(close(_).void).use(use)

    reqRes.raceSuccess(reqRes)
  }

/*
  def scenario9(client: Client[IO], scenarioUrl: Int => Uri) = {
    val req = for
      resp <- Client.request(scenarioUrl(9)).filterOrFail(_.status.isSuccess)(Error())
      body <- resp.body.asString
      now <- Clock.nanoTime
    yield
      now -> body

    ZIO.withParallelism(10) {
      ZIO.collectAllSuccessesPar(Seq.fill(10)(req)).map { resp =>
        resp.sortBy(_._1).map(_._2).mkString
      }
    }
  }
*/

  def scenario9(client: Client[IO], scenarioUrl: Int => Uri) = {
    val req = for {
      body <- client.expect[String](scenarioUrl(9))
      now <- Clock[IO].realTimeInstant
    } yield
      now -> body

    List.fill(10)(req)
      .parTraverseFilter(_.option)
      .map(_.sortBy(_._1).map(_._2).mkString)
  }

  def all(client: Client[IO], scenarioUrl: Int => Uri) =
    List(
      scenario1 _,
      scenario2 _,
      scenario3 _,
      scenario4 _,
      scenario5 _,
      scenario6 _,
      scenario7 _,
      scenario8 _,
      scenario9 _,
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
