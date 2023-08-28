import scala.concurrent.duration.*
import sttp.client3.*
import sttp.model.Uri

import ox.*


object EasyRacerClient:
  private val backend = HttpClientSyncBackend()
  private def scenarioRequest(uri: Uri): Request[String, Any] = basicRequest.get(uri).response(asStringAlways)

  def scenario1(scenarioUrl: Int => Uri): String =
    val url = scenarioUrl(1)
    // must be a def so that it's lazy-evaluated!
    def req = scenarioRequest(url).send(backend).body
    raceSuccess(req)(req)

  def scenario2(scenarioUrl: Int => Uri): String =
    val url = scenarioUrl(2)
    def req = scenarioRequest(url).send(backend)
    raceSuccess(req)(req).body

  def scenario3(scenarioUrl: Int => Uri): String =
    val url = scenarioUrl(3)
    val reqs = Seq.fill(10000) { () => scenarioRequest(url).send(backend) }
    raceSuccess(reqs).body

  def scenario4(scenarioUrl: Int => Uri): String =
    val url = scenarioUrl(4)
    def req = scenarioRequest(url).send(backend).body
    raceSuccess(timeout(1.second)(req))(req)

  def scenario5(scenarioUrl: Int => Uri): String =
    val url = scenarioUrl(5)
    def req = basicRequest.get(url).response(asString.getRight).send(backend).body
    raceSuccess(req)(req)

  def scenario6(scenarioUrl: Int => Uri): String =
    val url = scenarioUrl(6)
    def req = basicRequest.get(url).response(asString.getRight).send(backend).body
    raceSuccess(Seq(() => req, () => req, () => req))

  def scenario7(scenarioUrl: Int => Uri): String =
    val url = scenarioUrl(7)
    def req = scenarioRequest(url).send(backend).body
    raceSuccess(req) {
      Thread.sleep(4000)
      req
    }

  def scenario8(scenarioUrl: Int => Uri): String =
    def req(url: Uri) = basicRequest.get(url).response(asString.getRight).send(backend).body

    def open = req(uri"${scenarioUrl(8)}?open")
    def use(id: String) = req(uri"${scenarioUrl(8)}?use=$id")
    def close(id: String) = req(uri"${scenarioUrl(8)}?close=$id")

    def reqRes =
      val id = open
      try use(id)
      finally close(id)

    raceSuccess(reqRes)(reqRes)

  def scenario9(scenarioUrl: Int => Uri): String =
    def req =
      val body = basicRequest.get(scenarioUrl(9)).response(asString.getRight).send(backend).body
      val now = System.nanoTime
      now -> body

    scoped {
      val forks = Seq.fill(10)(fork(req))
      forks.map(_.joinEither()).collect { case Right(v) => v }.sortBy(_._1).map(_._2).mkString
    }

@main def run(): Unit =
  import EasyRacerClient.*
  def scenarioUrl(scenario: Int) = uri"http://localhost:8080/$scenario"
  def scenarios = Seq(scenario1, scenario2, scenario3, scenario4, scenario5, scenario6, scenario7, scenario8, scenario9)
  // def scenarios: Seq[(Int => Uri) => String] = Seq(scenario3)
  scenarios.foreach { s =>
    println(s(scenarioUrl))
  }
