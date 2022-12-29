import weaver._

import cats.effect._

import org.http4s.Uri

object EasyRacerSuite extends IOSuite {
  override type Res = EasyRacerContainer

  def sharedResource: Resource[IO,Res] = {
    ContainerResource(IO(EasyRacerContainer()))
  }

  test("easyracer") { racer =>
    val host = racer.host
    val port = racer.mappedPort(8080)

    EasyRacerClient.cr.use { client =>
      for {
        results <- EasyRacerClient.all(client, i => Uri.unsafeFromString(s"http://${host}:$port/$i"))
        ans      = expect(results.forall(_ == "right"))
      } yield ans
    }
  }
}
