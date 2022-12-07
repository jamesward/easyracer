import zio.*
import zio.http.*

import java.util.UUID
import java.util.concurrent.TimeoutException

object EasyRacerClient extends ZIOAppDefault:

  def scenario1(scenarioUrl: Int => String) =
    val url = scenarioUrl(1)
    val reqs = Seq.fill(10000)(Client.request(url))
    for
      winner <- ZIO.raceAll(reqs.head, reqs.tail)
      body <- winner.body.asString
    yield
      body


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
    val req = for
      resp <- Client.request(url)
      body <- resp.body.asString
    yield
      body

    req.timeoutFail(TimeoutException())(1.seconds).race(req)


  def scenario4(scenarioUrl: Int => String) =
    val url = scenarioUrl(4)
    val req = for
      resp <- Client.request(url).filterOrFail(_.status.isSuccess)(Error())
      body <- resp.body.asString
    yield
      body

    req.race(req)


  def scenario5(scenarioUrl: Int => String) =
    val url = scenarioUrl(5)
    val req = for
      resp <- Client.request(url)
      body <- resp.body.asString
    yield
      body

    // todo: sometimes the first req can take a second or 2 to start which can break the hedge check which verifies the second request starts 2 seconds after the first one
    //   but it isn't clear how to resolve that as there isn't a way to know when the req is connected, then send the second one
    req.race(ZIO.sleep(4.seconds) *> req)

  def scenarios(scenarioUrl: Int => String) = Seq(scenario1, scenario2, scenario4, scenario5).map(_.apply(scenarioUrl))
  //val scenarios = Seq(scenario5)
  def all(scenarioUrl: Int => String) = ZIO.collectAllPar(scenarios(scenarioUrl))

  override val run =
    def scenarioUrl(scenario: Int) = s"http://localhost:8080/$scenario"
    all(scenarioUrl).debug.filterOrDie(_.forall(_ == "right"))(Error()).provide(Client.default, Scope.default)
