import zio.*
import zio.http.*
import zio.http.model.Method
import zio.http.service.ChannelFactory

import java.io.IOException

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

  override val run =
    val scenarios = Seq(scenario1, scenario2)
    val done = for
      results <- ZIO.collectAll(scenarios)
    yield
      results

    done.debug.provide(Client.default, Scope.default)
