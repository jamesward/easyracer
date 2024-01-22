import zio.*
import zio.direct.*
import zio.http.*
import zio.test.*
import zio.test.Annotations.*
import com.dimafeng.testcontainers.GenericContainer
import org.testcontainers.containers.wait.strategy.Wait
import org.testcontainers.images.PullPolicy
import zio.http.netty.NettyConfig

import java.lang.management.ManagementFactory
import com.sun.management.OperatingSystemMXBean

import java.security.MessageDigest

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
  val clientConfig = Client.Config.default.disabledConnectionPool

  def spec = suite("easyracer")(
    test("all") {
      defer:
        /*
        for
          containerWrapper <- ZIO.service[GenericContainer]
          port = containerWrapper.container.getFirstMappedPort
          results <- EasyRacerClient.all({ i => s"http://localhost:$port/$i" }).provide(
            ZLayer.succeed(clientConfig),
            Client.live,
            ZLayer.succeed(NettyConfig.default),
            DnsResolver.default,
            Scope.default,
          )
        yield
          assertTrue(results.forall(_ == "right"))

         */
        val messageDigest = MessageDigest.getInstance("SHA-512")
        val seed = Random.nextBytes(512).run

        val blocking = ZIO.attemptBlockingInterrupt:
          var result = seed.toArray
          while (!Thread.interrupted())
            result = messageDigest.digest(result)

        val reporter = ZIO.attempt:
          val osBean = ManagementFactory.getPlatformMXBean(classOf[OperatingSystemMXBean])
          println(osBean.getProcessCpuLoad * osBean.getAvailableProcessors)

        reporter.repeat(Schedule.spaced(100.millis).upTo(5.seconds)).race(blocking).run


        assertTrue(false)
    } @@ TestAspect.withLiveClock
  )//.provideLayerShared(containerLayer)
