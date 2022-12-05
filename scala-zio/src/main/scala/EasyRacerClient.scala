import zio.*
import zio.http.*

import java.util.concurrent.TimeoutException

object EasyRacerClient extends ZIOAppDefault:

  def scenarioUrl(scenario: Int) = s"http://localhost:8080/$scenario?user=2"

  val scenario1 =
    val url = scenarioUrl(1)
    val reqs = Seq.fill(10000)(Client.request(url))
    for
      winner <- ZIO.raceAll(reqs.head, reqs.tail)
      body <- winner.body.asString
    yield
      body


  val scenario2 =
    val url = scenarioUrl(2)
    val req = for
      resp <- Client.request(url)
      body <- resp.body.asString
    yield
      body

    req.race(req)


  val scenario3 =
    val url = scenarioUrl(3)
    val req = for
      resp <- Client.request(url)
      body <- resp.body.asString
    yield
      body

    req.timeoutFail(TimeoutException())(1.seconds).race(req)


  val scenario4 =
    val url = scenarioUrl(4)
    val req = for
      resp <- Client.request(url).filterOrFail(_.status.isSuccess)(Error())
      body <- resp.body.asString
    yield
      body

    req.race(req)


  override val run =
    val scenarios = Seq(scenario1, scenario2, scenario4)
    //val scenarios = Seq(scenario4)
    val all = ZIO.collectAllPar(scenarios).filterOrDie(_.forall(_ == "right"))(Error())
    all.provide(Client.default, Scope.default)
