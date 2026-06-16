import com.sun.management.OperatingSystemMXBean
import kyo.*

import scala.collection.immutable.SortedMap

import java.lang.management.ManagementFactory
import java.security.MessageDigest


object EasyRacerClient extends KyoApp:

  // The default HttpClient config has a 5s request timeout and follows redirects, neither of which works
  // for these scenarios: scenarios 7 and 10 hold the connection open for many seconds, and scenario 10
  // relies on observing 3xx responses from the reporter without auto-following them.
  private inline def withClient[A, S](v: A < (Async & Abort[HttpException] & S)): A < (Async & Abort[HttpException] & S) =
    HttpClient.withConfig(_.timeout(Duration.Infinity).followRedirects(false))(v)

  def scenario1(scenarioUrl: Int => String) =
    val url = scenarioUrl(1)
    val req = HttpClient.getText(url)
    Async.race(req, req)

  def scenario2(scenarioUrl: Int => String) =
    val url = scenarioUrl(2)
    val req = HttpClient.getText(url)
    Async.race(req, req)

  def scenario3(scenarioUrl: Int => String) =
    val url = scenarioUrl(3)
    val reqs = Seq.fill(10000):
      HttpClient.getText(url)
    Async.race(reqs)

  def scenario4(scenarioUrl: Int => String) =
    val url = scenarioUrl(4)
    val req = HttpClient.getText(url)
    val reqWithTimeout = Async.timeout(1.seconds)(req)
    Async.race(req, reqWithTimeout)

  def scenario5(scenarioUrl: Int => String) =
    val url = scenarioUrl(5)
    val req = HttpClient.getText(url)
    Async.race(req, req)

  def scenario6(scenarioUrl: Int => String) =
    val url = scenarioUrl(6)
    val req = HttpClient.getText(url)
    Async.race(req, req, req)

  def scenario7(scenarioUrl: Int => String) =
    val url = scenarioUrl(7)
    val req = HttpClient.getText(url)
    val delayedReq = Async.delay(4.seconds)(req)
    Async.race(req, delayedReq)

  def scenario8(scenarioUrl: Int => String): String < (Abort[HttpException] & Async) =
    val baseUrl = scenarioUrl(8)
    case class MyResource(id: String):
      def close: Unit < Async =
        Abort.run(HttpClient.getText(s"$baseUrl?close=$id")).unit

    val myResource = direct:
      val id = HttpClient.getText(s"$baseUrl?open").now
      Scope.acquireRelease(MyResource(id))(_.close).now

    val reqRes =
      Scope.run:
        direct:
          val resource: MyResource = myResource.now
          HttpClient.getText(s"$baseUrl?use=${resource.id}").now

    Async.race(reqRes, reqRes)

  def scenario9(scenarioUrl: Int => String) =
    val url = scenarioUrl(9)

    val req =
        direct:
          val body = HttpClient.getText(url).now
          val now = Clock.now.now
          now -> body

    val reqs = Seq.fill(10)(req)

    direct:
      val successes = SortedMap.from(Async.gather(reqs).now)
      successes.values.mkString

  def scenario10(scenarioUrl: Int => String) =
    val baseUrl = scenarioUrl(10)
    val messageDigest = MessageDigest.getInstance("SHA-512")

    // recursive digesting
    def blocking(bytesEffect: Seq[Byte] < (Abort[HttpException] & Async)): Seq[Byte] < (Abort[HttpException] & Async) =
      Sync.defer:
        bytesEffect.map: bytes =>
          blocking(messageDigest.digest(bytes.toArray).toSeq)

    // runs blocking code while the request is open
    def blocker(id: String): String < (Abort[HttpException] & Async) =
      Async.race(
        HttpClient.getText(s"$baseUrl?$id"),
        blocking(Random.nextBytes(512)).map(_ => ""),
      )

    // sends CPU usage every second until the server says to stop
    def reporter(id: String): String < (Abort[HttpException] & Async) =
      val osBean = ManagementFactory.getPlatformMXBean(classOf[OperatingSystemMXBean])
      val load = osBean.getProcessCpuLoad * osBean.getAvailableProcessors

      direct:
        val resp = HttpClient.getTextResponse(s"$baseUrl?$id=$load", failOnError = false).now
        val status = resp.status
        val body = resp.fields.body

        if status.isRedirect then
          Async.delay(1.seconds)(reporter(id)).now
        else if status.isSuccess then
          body
        else
          Abort.fail(HttpStatusException(status, "GET", baseUrl, body)).now

    direct:
      val id = Random.nextStringAlphanumeric(8).now
      val (_, result) = Async.zip(blocker(id), reporter(id)).now
      result

  def scenario11(scenarioUrl: Int => String) =
    val url = scenarioUrl(11)
    val req = HttpClient.getText(url)
    Async.race(Async.race(req, req), req)

  def scenarioUrl(scenario: Int) = s"http://localhost:8080/$scenario"

  def scenarios = Seq(scenario1, scenario2, scenario3, scenario4, scenario5, scenario6, scenario7, scenario8, scenario9, scenario10, scenario11)

  run:
    withClient:
      Kyo.collectAll(scenarios.map(s => s(scenarioUrl)))
