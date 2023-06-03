import zio.*
import zio.http.*
import zio.test.*
import zio.test.Annotations.*
import zio.test.Assertion.*
import com.dimafeng.testcontainers.GenericContainer
import org.testcontainers.containers.wait.strategy.Wait
import org.testcontainers.images.PullPolicy
import zio.http.netty.NettyConfig

import java.io.IOException

object EasyRacerClientSpec extends ZIOSpecDefault:

  val container = ZIO.acquireRelease {
    ZIO.attempt {
      val container = GenericContainer("ghcr.io/jamesward/easyracer", Seq(8080), waitStrategy = Wait.forHttp("/"), imagePullPolicy = PullPolicy.alwaysPull())
      container.start()
      container
    }.orDie
  } { container =>
    ZIO
      .attempt(container.stop())
      .ignoreLogged
  }

  val containerLayer = ZLayer.scoped(container)
  val clientConfig = Client.Config.default.withDisabledConnectionPool

  def spec = suite("easyracer")(
    test("all") {
      for
        containerWrapper <- ZIO.service[GenericContainer]
        port = containerWrapper.container.getFirstMappedPort
        results <- EasyRacerClient.all({ i => s"http://localhost:$port/$i" }).provide(
          ZLayer.succeed(clientConfig),
          Client.live,
          ZLayer.succeed(NettyConfig.default),
          DnsResolver.default,
        )
      yield
        assertTrue(results.forall(_ == "right"))
    } @@ TestAspect.withLiveClock
  ).provideLayerShared(containerLayer)
