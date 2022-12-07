import zio.*
import zio.http.*
import zio.http.model.Method
import zio.test.*
import zio.test.Annotations.*
import zio.test.Assertion.*
import com.dimafeng.testcontainers.GenericContainer
import java.io.IOException

object EasyRacerClientSpec extends ZIOSpecDefault:

  val container = ZIO.acquireRelease {
    ZIO.attempt {
      val container = GenericContainer("ghcr.io/jamesward/easyracer", Seq(8080))
      container.start()
      container
    }.orDie
  } { container =>
    ZIO
      .attempt(container.stop())
      .ignoreLogged
  }

  val containerLayer = ZLayer.scoped(container)

  def spec = suite("easyracer")(
    test("all") {
      for
        containerWrapper <- ZIO.service[GenericContainer]
        port = containerWrapper.container.getFirstMappedPort
        f <- EasyRacerClient.all({ i => s"http://localhost:$port/$i" }).provide(Client.default, Scope.default).fork
        _ <- TestClock.adjust(10.seconds)
        results <- f.join
      yield
        assertTrue(results.forall(_ == "right"))
    }
  ).provideLayerShared(containerLayer)
