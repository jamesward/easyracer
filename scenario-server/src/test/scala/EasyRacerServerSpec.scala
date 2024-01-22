import EasyRacerServer.Scenario10Data
import com.sun.management.OperatingSystemMXBean
import zio.http.netty.NettyConfig
import zio.{Schedule, *}
import zio.direct.*
import zio.http.{Client, DnsResolver, Handler, Request, Server, URL}
import zio.test.*
import zio.test.Assertion.*

import java.lang.management.ManagementFactory
import java.security.MessageDigest

object EasyRacerServerSpec extends ZIOSpecDefault:

  def spec = suite("EasyRacerServer")(
    test("Scenario1") {
      defer:
        val session = EasyRacerServer.Session.make[Unit]().run
        val req = EasyRacerServer.scenario1(session)(Request.get(URL.empty))
        val winner = req.race(req).run
        val body = winner.body.asString.run
        assertTrue(body == "right")
    },

    test("Scenario1 - Actual Server") {
      defer:
        val session = EasyRacerServer.Session.make[Unit]().run
        Server.serve(Handler.fromFunctionZIO(EasyRacerServer.scenario1(session)).toHttpApp).forkScoped.run
        val server = ZIO.service[Server].run
        val req = Client.request(Request.get(s"http://localhost:${server.port}/1"))
        val winner = req.race(req).run
        val body = winner.body.asString.run
        assertTrue(body == "right")
    }.provide(
      Server.defaultWithPort(0),
      Client.default,
      Scope.default,
    ),

    test("Scenario3 - Actual Server") {
      defer:
        val session = EasyRacerServer.Session.make[Unit]().run
        Server.serve(Handler.fromFunctionZIO(EasyRacerServer.scenario3(session)).toHttpApp).forkScoped.run
        val server = ZIO.service[Server].run
        val reqs = Seq.fill(10000)(Client.request(Request.get(s"http://localhost:${server.port}/3")))
        val winner = ZIO.raceAll(reqs.head, reqs.tail).run
        val body = winner.body.asString.run
        TestClock.adjust(1.minute).run // todo: Something with DnsResolver seems to be hanging this test unless we move the clock forward
        assertTrue(body == "right")
    }.provide(
      Server.defaultWithPort(0),
      ZLayer.succeed(Client.Config.default.disabledConnectionPool),
      Client.live,
      ZLayer.succeed(NettyConfig.default),
      DnsResolver.default,
      Scope.default,
    ),
  ).provideLayer(Runtime.removeDefaultLoggers)
