import weaver._

import cats.implicits._

import cats.effect._

import org.http4s.Uri
import org.http4s.client.Client
import org.http4s.ember.client.EmberClientBuilder

import EasyRacerClient.all

object EasyRacerSuite extends IOSuite {
  override type Res = (EasyRacerContainer, Client[IO])

  def sharedResource: Resource[IO,Res] = {
    (
      ContainerResource(IO(EasyRacerContainer())),
      EmberClientBuilder.default[IO].build
    ).parTupled
  }

  test("easyracer") { case (racer, client) =>
    val host   = racer.host
    val port   = racer.mappedPort(8080)

    val ios    = all(client, i => Uri.unsafeFromString(s"http://${host}:$port/$i"))
    val result = ios.map { _.forall(_ == "right") }

    result.map(b => expect(b))
  }
}
