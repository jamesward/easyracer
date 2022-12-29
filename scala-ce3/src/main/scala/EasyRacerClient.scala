import cats._, cats.implicits._

import cats.effect._, cats.effect.implicits._

import org.http4s._, org.http4s.implicits._
import org.http4s.Method._

import org.http4s.client.Client
import org.http4s.client.dsl.io._
import org.http4s.ember.client._

object EasyRacerClient extends IOApp.Simple {
  val cr = EmberClientBuilder.default[IO].build

  def scenario1(client: Client[IO], scenarioUrl: Int => Uri) = {
    val url = scenarioUrl(1)
    val req = client.expect[String](GET(url))
 
    req.race(req).map(_.merge)
  }

  def all(client: Client[IO], scenarioUrl: Int => Uri) =
    List((scenario1 _) /*, 
      (scenario2 _),
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
    def scenarioUrl(scenario: Int) = Uri.unsafeFromString(s"http://localhost:8080/$scenario")

    cr.use { client =>
      all(client, scenarioUrl)
        .map(_.forall(_ == "right"))
    }
  }
}
