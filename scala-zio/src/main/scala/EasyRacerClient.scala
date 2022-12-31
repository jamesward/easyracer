import zio.*
import zio.http.*

import java.util.UUID
import java.util.concurrent.TimeoutException

// Note that code is intentionally NOT shared across different scenarios, except for the url / config handling
object EasyRacerClient extends ZIOAppDefault:

  def scenario1(scenarioUrl: Int => String) =
    val url = scenarioUrl(1)
    val req = for
      resp <- Client.request(url)
      body <- resp.body.asString
    yield
      body

    req.race(req)

  def scenario2(scenarioUrl: Int => String) =
    val url = scenarioUrl(2)
    val req = for
      resp <- Client.request(url)
      body <- resp.body.asString
    yield
      body

    req.race(req)


  def scenario3(scenarioUrl: Int => String) =
    val url = scenarioUrl(3)
    val reqs = Seq.fill(10000)(Client.request(url))
    for
      winner <- ZIO.raceAll(reqs.head, reqs.tail)
      body <- winner.body.asString
    yield
      body


  def scenario4(scenarioUrl: Int => String) =
    val url = scenarioUrl(4)
    val req = for
      resp <- Client.request(url)
      body <- resp.body.asString
    yield
      body

    req.timeoutFail(TimeoutException())(1.seconds).race(req)


  def scenario5(scenarioUrl: Int => String) =
    val url = scenarioUrl(5)
    val req = for
      resp <- Client.request(url).filterOrFail(_.status.isSuccess)(Error())
      body <- resp.body.asString
    yield
      body

    req.race(req)


  def scenario6(scenarioUrl: Int => String) =
    val url = scenarioUrl(6)
    val req = for
      resp <- Client.request(url).filterOrFail(_.status.isSuccess)(Error())
      body <- resp.body.asString
    yield
      body

    ZIO.raceAll(req, Seq(req, req))


  def scenario7(scenarioUrl: Int => String) =
    val url = scenarioUrl(7)
    val req = for
      resp <- Client.request(url)
      body <- resp.body.asString
    yield
      body

    // todo: sometimes the first req can take a second or 2 to start which can break the hedge check which verifies the second request starts 2 seconds after the first one
    //   but it isn't clear how to resolve that as there isn't a way to know when the req is connected, then send the second one
    req.race(ZIO.sleep(4.seconds) *> req)


  def scenario8(scenarioUrl: Int => String) =
    def req(url: String) = for
      resp <- Client.request(url).filterOrFail(_.status.isSuccess)(Error())
      body <- resp.body.asString
    yield
      body

    val open = req(scenarioUrl(8) + "?open")
    def use(id: String) = req(scenarioUrl(8) + s"?use=$id")
    def close(id: String) = req(scenarioUrl(8) + s"?close=$id")

    val reqRes = ZIO.acquireReleaseWith(open)(close(_).orDie)(use)

    reqRes.race(reqRes)


  def scenario9(scenarioUrl: Int => String) =
    val req = for
      resp <- Client.request(scenarioUrl(9)).filterOrFail(_.status.isSuccess)(Error())
      body <- resp.body.asString
      now <- Clock.nanoTime
    yield
      now -> body

    ZIO.withParallelism(10) {
      ZIO.collectAllSuccessesPar(Seq.fill(10)(req)).map { resp =>
        resp.sortBy(_._1).map(_._2).mkString
      }
    }


  def scenarios(scenarioUrl: Int => String) = Seq(scenario1, scenario2, scenario3, scenario4, scenario5, scenario6, scenario7, scenario8, scenario9).map(_.apply(scenarioUrl))
  //def scenarios(scenarioUrl: Int => String) = Seq(scenario9).map(_.apply(scenarioUrl))
  def all(scenarioUrl: Int => String) = ZIO.collectAllPar(scenarios(scenarioUrl))

  override val run =
    def scenarioUrl(scenario: Int) = s"http://localhost:8080/$scenario"
    all(scenarioUrl).debug.filterOrDie(_.forall(_ == "right"))(Error("not all right")).provide(Client.default, Scope.default)
