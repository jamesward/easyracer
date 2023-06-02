import zio.*
import zio.concurrent.ConcurrentSet
import zio.http.*
import zio.direct.*
import zio.http.netty.NettyConfig

import java.util.UUID
import java.util.concurrent.TimeoutException
import scala.collection.immutable.SortedMap
import scala.annotation.tailrec

// Note that code is intentionally NOT shared across different scenarios, except for the url / config handling
object EasyRacerClient extends ZIOAppDefault:

  def scenario1(scenarioUrl: Int => String) =
    defer:
      val url = scenarioUrl(1)
      val req = Client.request(url)
      val winner = req.race(req).run
      winner.body.asString.run


  def scenario2(scenarioUrl: Int => String) =
    defer:
      val url = scenarioUrl(2)
      val req = Client.request(url)
      val winner = req.race(req).run
      winner.body.asString.run


  def scenario3(scenarioUrl: Int => String) =
    defer:
      val url = scenarioUrl(3)
      val reqs = Seq.fill(10000)(Client.request(url))
      val winner = ZIO.raceAll(reqs.head, reqs.tail).run
      winner.body.asString.run


  def scenario4(scenarioUrl: Int => String) =
    defer:
      val url = scenarioUrl(4)
      val req = Client.request(url)
      val winner = req.timeoutFail(TimeoutException())(1.seconds).race(req).run
      winner.body.asString.run


  def scenario5(scenarioUrl: Int => String) =
    defer:
      val url = scenarioUrl(5)
      val req = Client.request(url).filterOrFail(_.status.isSuccess)(Error())
      val winner = req.race(req).run
      winner.body.asString.run


  def scenario6(scenarioUrl: Int => String) =
    defer:
      val url = scenarioUrl(6)
      val req = Client.request(url).filterOrFail(_.status.isSuccess)(Error())
      val winner = ZIO.raceAll(req, Seq(req, req)).run
      winner.body.asString.run


  def scenario7(scenarioUrl: Int => String) =
    defer:
      val url = scenarioUrl(7)
      val req = Client.request(url)
      // todo: sometimes the first req can take a second or 2 to start which can break the hedge check which verifies the second request starts 2 seconds after the first one
      //   but it isn't clear how to resolve that as there isn't a way to know when the req is connected, then send the second one
      val winner = req.race(req.delay(4.seconds)).run
      winner.body.asString.run


  def scenario8(scenarioUrl: Int => String) =
    def req(url: String) =
      defer:
        val resp = Client.request(url).filterOrFail(_.status.isSuccess)(Error()).run
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
        val resp = Client.request(url).filterOrFail(_.status.isSuccess)(Error()).run
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


  // in-progress below here

  def fibonacci(n: Int): BigInt = {
    @tailrec
    def fibonacciTail(n: Int, a: BigInt, b: BigInt): BigInt = {
      if (n == 0) a
      else if (n == 1) b
      else fibonacciTail(n - 1, b, a + b)
    }

    fibonacciTail(n, 0, 1)
  }


  def scenario10(scenarioUrl: Int => String) =
    def req(params: Option[String] = None) = for
      resp <- Client.request(scenarioUrl(10) + params.getOrElse("")).filterOrFail(_.status.isSuccess)(Error())
      body <- resp.body.asString
    yield
      body

    for
      num1 <- req(None)
      fib1 = fibonacci(num1.toInt)
      num2 <- req(Some(s"?$num1=$fib1"))
      fib2 = fibonacci(num2.toInt)
      resp <- req(Some(s"?$num2=$fib2"))
    yield
      resp


  def scenario11(scenarioUrl: Int => String) =
    def req(params: Option[String] = None) = for
      resp <- Client.request(scenarioUrl(11) + params.getOrElse(""))
      body <- resp.body.asString
    yield
      body

    for
      nums <- req(None)
      Array(num1, num2) = nums.split(',')
      fibFiber1 <- ZIO.succeedBlocking(fibonacci(num1.toInt)).fork
      fibFiber2 <- ZIO.succeedBlocking(fibonacci(num2.toInt)).fork
      fib1 <- fibFiber1.join
      fib2 <- fibFiber2.join
      resp <- req(Some(s"?$num1=$fib1&$num2=$fib2"))
    yield
      resp


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
    //scenario10,
    //scenario11,
  ).map(_.apply(scenarioUrl))

  //def scenarios(scenarioUrl: Int => String) = Seq(scenario9).map(_.apply(scenarioUrl))
  def all(scenarioUrl: Int => String) = ZIO.collectAll(scenarios(scenarioUrl))

  val clientConfig: ZClient.Config = Client.Config.default.withDisabledConnectionPool //.withFixedConnectionPool(10000)

  override val run =
    def scenarioUrl(scenario: Int) = s"http://localhost:8080/$scenario"
    all(scenarioUrl).debug.filterOrDie(_.forall(_ == "right"))(Error("not all right")).provide(
      ZLayer.succeed(clientConfig),
      Client.live,
      ZLayer.succeed(NettyConfig.default),
      DnsResolver.default,
    )
