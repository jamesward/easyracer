import weaver._

import cats.implicits._

import cats.effect._

import org.http4s.Uri
import org.http4s.client.Client
import org.http4s.ember.client.EmberClientBuilder

import EasyRacerClient._

object EasyRacerSuite extends IOSuite {
  override type Res = (EasyRacerContainer, Client[IO])

  def sharedResource: Resource[IO,Res] = {
    (
      ContainerResource(IO(EasyRacerContainer())),
      EmberClientBuilder.default[IO].build
    ).parTupled
  }

  def scenarioUrl(racer: EasyRacerContainer)(scenario: Int): Uri =
    Uri.unsafeFromString(s"http://${racer.host}:${racer.mappedPort(8080)}/$scenario")

  test("scenario1") { case (racer, client) =>
    scenario1(client, scenarioUrl(racer)).map(s => expect(s == "right"))
  }

  test("scenario2") { case (racer, client) =>
    scenario2(client, scenarioUrl(racer)).map(s => expect(s == "right"))
  }

  test("scenario3") { case (racer, client) =>
    scenario3(client, scenarioUrl(racer)).map(s => expect(s == "right"))
  }
}
