import zio.*
import zio.direct.*
import zio.http.netty.NettyConfig
import zio.http.*
import zio.test.*
import zio.test.Assertion.*

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
        Server.serve(Handler.fromFunctionZIO(EasyRacerServer.scenario1(session)).toRoutes).forkScoped.run
        val server = ZIO.service[Server].run
        val port = server.port.run
        val url = ZIO.fromEither(URL.decode(s"http://localhost:$port/1")).run
        val req = Client.batched(Request.get(url))
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
        Server.serve(Handler.fromFunctionZIO(EasyRacerServer.scenario3(session)).toRoutes).forkScoped.run
        val server = ZIO.service[Server].run
        val port = server.port.run
        val url = ZIO.fromEither(URL.decode(s"http://localhost:$port/3")).run
        val reqs = Seq.fill(10000)(Client.batched(Request.get(url)))
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
