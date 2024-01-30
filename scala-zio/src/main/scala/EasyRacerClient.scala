import com.sun.management.OperatingSystemMXBean
import zio.{Schedule, *}
import zio.http.*
import zio.direct.*
import zio.http.netty.NettyConfig

import java.lang.management.ManagementFactory
import java.security.MessageDigest
import java.util.concurrent.TimeoutException
import scala.collection.immutable.SortedMap
import scala.annotation.tailrec

// Note that code is intentionally NOT shared across different scenarios, except for the url / config handling
object EasyRacerClient extends ZIOAppDefault:

  def scenario1(scenarioUrl: Int => String) =
    defer:
      val url = scenarioUrl(1)
      val req = Client.request(Request.get(url))
      val winner = req.race(req).run
      winner.body.asString.run


  def scenario2(scenarioUrl: Int => String) =
    defer:
      val url = scenarioUrl(2)
      val req = Client.request(Request.get(url))
      val winner = req.race(req).run
      winner.body.asString.run


  def scenario3(scenarioUrl: Int => String) =
    defer:
      val url = scenarioUrl(3)
      val reqs = Seq.fill(10000)(Client.request(Request.get(url)))
      val winner = ZIO.raceAll(reqs.head, reqs.tail).run
      winner.body.asString.run


  def scenario4(scenarioUrl: Int => String) =
    defer:
      val url = scenarioUrl(4)
      val req = Client.request(Request.get(url))
      val winner = req.timeoutFail(TimeoutException())(1.seconds).race(req).run
      winner.body.asString.run


  def scenario5(scenarioUrl: Int => String) =
    defer:
      val url = scenarioUrl(5)
      val req = Client.request(Request.get(url)).filterOrFail(_.status.isSuccess)(Error())
      val winner = req.race(req).run
      winner.body.asString.run


  def scenario6(scenarioUrl: Int => String) =
    defer:
      val url = scenarioUrl(6)
      val req = Client.request(Request.get(url)).filterOrFail(_.status.isSuccess)(Error())
      val winner = ZIO.raceAll(req, Seq(req, req)).run
      winner.body.asString.run


  def scenario7(scenarioUrl: Int => String) =
    defer:
      val url = scenarioUrl(7)
      val req = Client.request(Request.get(url))
      // todo: sometimes the first req can take a second or 2 to start which can break the hedge check which verifies the second request starts 2 seconds after the first one
      //   but it isn't clear how to resolve that as there isn't a way to know when the req is connected, then send the second one
      val winner = req.race(req.delay(4.seconds)).run
      winner.body.asString.run


  def scenario8(scenarioUrl: Int => String) =
    def req(url: String) =
      defer:
        val resp = Client.request(Request.get(url)).filterOrFail(_.status.isSuccess)(Error()).run
        resp.body.asString.run

    val open = req(scenarioUrl(8) + "?open")
    def use(id: String) = req(scenarioUrl(8) + s"?use=$id")
    def close(id: String) = req(scenarioUrl(8) + s"?close=$id")

    val reqRes = ZIO.acquireReleaseWith(open)(close(_).orDie)(use)

    reqRes.race(reqRes)


  def scenario9(scenarioUrl: Int => String) =
    val req =
      defer:
        val url = scenarioUrl(9)
        val resp = Client.request(Request.get(url)).filterOrFail(_.status.isSuccess)(Error()).run
        val body = resp.body.asString.run
        val now = Clock.nanoTime.run
        now -> body

    defer(Use.withParallelEval):
      val responses = Queue.unbounded[(Long, String)].run
      for _ <- 1 to 10 do
        req.option.run match
          case Some(resp) => responses.offer(resp).run
          case None => ()

      responses.takeAll.run.to(SortedMap).values.mkString


  def scenario10(scenarioUrl: Int => String) =

    // not using defer due to recursion issue
    def reporter(id: String): ZIO[Client & Scope, Throwable, String] =
      val osBean = ManagementFactory.getPlatformMXBean(classOf[OperatingSystemMXBean])
      val load = osBean.getProcessCpuLoad * osBean.getAvailableProcessors
      Client.request(Request.get(scenarioUrl(10) + s"?$id=$load")).flatMap: resp =>
        if resp.status.isRedirection then
          ZIO.sleep(1.second) *> reporter(id)
        else if resp.status.isSuccess then
          resp.body.asString
        else
          resp.body.asString.flatMap: body =>
            ZIO.fail(Error(body))

    defer:
      val id = Random.nextString(8).run
      val messageDigest = MessageDigest.getInstance("SHA-512")
      val seed = Random.nextBytes(512).run

      val blocking = ZIO.attemptBlockingInterrupt:
        var result = seed.toArray
        while (!Thread.interrupted())
          result = messageDigest.digest(result)

      val blocker =
        Client.request(Request.get(scenarioUrl(10) + s"?$id")).race(blocking) *> ZIO.never

      blocker.fork.run
      reporter(id).run


  def scenarios(scenarioUrl: Int => String) = Seq(
    scenario1,
    scenario2,
    scenario3,
    scenario4,
    scenario5,
    scenario6,
    scenario7,
    scenario8,
    scenario9,
    scenario10,
  ).map(_.apply(scenarioUrl))

  //def scenarios(scenarioUrl: Int => String) = Seq(scenario9).map(_.apply(scenarioUrl))
  def all(scenarioUrl: Int => String) = ZIO.collectAll(scenarios(scenarioUrl))

  val clientConfig = Client.Config.default.disabledConnectionPool

  override val run =
    def scenarioUrl(scenario: Int) = s"http://localhost:8080/$scenario"
    all(scenarioUrl).debug.filterOrDie(_.forall(_ == "right"))(Error("not all right")).provide(
      ZLayer.succeed(clientConfig),
      ZLayer.succeed(NettyConfig.default),
      DnsResolver.default,
      Client.live,
      Scope.default,
    )
