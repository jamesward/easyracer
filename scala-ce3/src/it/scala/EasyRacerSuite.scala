import weaver._

import scala.concurrent.duration._

import cats.implicits._

import cats.effect._

import org.http4s.Uri
import org.http4s.client.Client

import EasyRacerClient._

object EasyRacerSuite extends IOSuite {
  override type Res = (EasyRacerContainer, Client[IO])

  def sharedResource: Resource[IO,Res] = {
    (
      ContainerResource(IO(EasyRacerContainer())),
      cr
    ).parTupled
  }

  def scenarioUrl(racer: EasyRacerContainer)(scenario: Int): Uri =
    Uri.unsafeFromString(s"http://${racer.host}:${racer.mappedPort(8080)}/$scenario")

  test("scenario1") { case (racer, client) =>
    scenario1(client, scenarioUrl(racer)).map(s => expect(s === "right"))
  }

  test("scenario2") { case (racer, client) =>
    scenario2(client, scenarioUrl(racer)).map(s => expect(s === "right"))
  }

  test("scenario3") { case (racer, client) =>
    scenario3(client, scenarioUrl(racer)).map(s => expect(s === "right"))
  }

  test("scenario4") { case (racer, client) =>
    scenario4(client, scenarioUrl(racer)).map(s => expect(s === "right"))
  }

  test("scenario5") { case (racer, client) =>
    scenario5(client, scenarioUrl(racer)).map(s => expect(s === "right"))
  }

  test("scenario6") { case (racer, client) =>
    scenario6(client, scenarioUrl(racer)).map(s => expect(s === "right"))
  }

  test("scenario7") { case (racer, client) =>
    scenario7(client, scenarioUrl(racer)).map(s => expect(s === "right"))
  }

  test("scenario8") { case (racer, client) =>
    scenario8(client, scenarioUrl(racer)).map(s => expect(s === "right"))
  }

  test("scenario9") { case (racer, client) =>
    scenario9(client, scenarioUrl(racer)).map(s => expect(s === "right"))
  }
}
