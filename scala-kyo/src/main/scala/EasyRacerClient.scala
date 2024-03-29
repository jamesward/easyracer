import com.sun.management.OperatingSystemMXBean
import kyo.*
import kyo.Fibers.Effects
import sttp.client3.*
import sttp.model.Uri.QuerySegment
import sttp.model.{ResponseMetadata, Uri}

import java.io.Closeable
import java.lang.management.ManagementFactory
import java.security.MessageDigest
import java.time.Instant
import scala.concurrent.duration.*


object EasyRacerClient extends KyoApp:

  def scenario1(scenarioUrl: Int => Uri): String < Fibers =
    val url = scenarioUrl(1)
    val req = Requests[String](_.get(url))
    Fibers.race(req, req)

  def scenario2(scenarioUrl: Int => Uri): String < Fibers =
    val url = scenarioUrl(2)
    val req = Requests[String](_.get(url))
    Fibers.raceSuccesses(req, req)

  def scenario3(scenarioUrl: Int => Uri): String < Fibers =
    val url = scenarioUrl(3)
    val reqs = Seq.fill(10000):
      Requests[String](_.get(url))
    Fibers.race(reqs)

  def scenario4(scenarioUrl: Int => Uri): String < Fibers =
    val url = scenarioUrl(4)
    val req = Requests[String](_.get(url))
    Fibers.raceSuccesses(Fibers.timeout(1.second)(req), req)

  def scenario5(scenarioUrl: Int => Uri): String < Fibers =
    val url = scenarioUrl(5)
    val req = Requests[String](_.get(url))
    Fibers.raceSuccesses(req, req)

  def scenario6(scenarioUrl: Int => Uri): String < Fibers =
    val url = scenarioUrl(6)
    val req = Requests[String](_.get(url))
    Fibers.raceSuccesses(Seq(req, req, req))

  def scenario7(scenarioUrl: Int => Uri): String < Fibers =
    val url = scenarioUrl(7)
    val req = Requests[String](_.get(url))
    val delayedReq = Fibers.delay(4.seconds)(req)
    Fibers.race(req, delayedReq)

  def scenario8(scenarioUrl: Int => Uri): String < Fibers =
    def req(uri: Uri) = Requests[String](_.get(uri))

    case class MyResource(id: String) extends Closeable:
      override def close(): Unit =
        IOs.run(Fibers.run(req(uri"${scenarioUrl(8)}?close=$id")))

    val myResource = defer:
      val id = await:
        req(uri"${scenarioUrl(8)}?open")
      await:
        Resources.acquire(MyResource(id))

    val reqRes = Resources.run:
      defer:
        val resource = await(myResource)
        await:
          req(uri"${scenarioUrl(8)}?use=${resource.id}")

    Fibers.raceSuccesses(reqRes, reqRes)

  def scenario9(scenarioUrl: Int => Uri): String < Fibers =
    val url = scenarioUrl(9)
    val req = IOs.attempt:
      defer:
        val body = await(Requests[String](_.get(url)))
        val now = await(Clocks.now)
        now -> body

    val reqs = Seq.fill(10)(req)

    defer:
      val successes = await(Fibers.parallel(reqs)).collect:
        case scala.util.Success(value) => value

      successes.sortBy(_._1).map(_._2).mkString

  def scenario10(scenarioUrl: Int => Uri): String < Fibers =
    // always puts the body into the response, even if it is empty, and includes the responseMetadata
    def req(uriModifier: Uri => Uri): (String, ResponseMetadata) < Fibers =
      val uri = uriModifier(scenarioUrl(10))
      Requests[(String, ResponseMetadata)]:
        _.get(uri).response:
          asString.mapWithMetadata: (stringEither, responseMetadata) =>
            Right(stringEither.fold(identity, identity) -> responseMetadata)

    val messageDigest = MessageDigest.getInstance("SHA-512")

    // recursive digesting
    def blocking(bytesEffect: Array[Byte] < Fibers): String < (Fibers & IOs) =
      IOs {
        bytesEffect.map { bytes =>
          blocking(messageDigest.digest(bytes))
        }
      }

    // runs blocking code while the request is open
    def blocker(id: String): String < Fibers =
      Fibers.race(
        req(_.addQuerySegment(QuerySegment.Plain(id))).map(_._1),
        blocking(Randoms.nextBytes(512)),
      )

    // sends CPU usage every second until the server says to stop
    def reporter(id: String): String < Fibers =
      val osBean = ManagementFactory.getPlatformMXBean(classOf[OperatingSystemMXBean])
      val load = osBean.getProcessCpuLoad * osBean.getAvailableProcessors

      defer:
        val (maybeResponseBody, responseMetadata) =
          await(req(_.addQuerySegment(QuerySegment.KeyValue(id, load.toString))))

        if responseMetadata.isRedirect then
          await(Fibers.delay(1.seconds)(reporter(id)))
        else if responseMetadata.isSuccess then
          maybeResponseBody
        else
          await(Fibers.get(Fibers.fail(Error(maybeResponseBody))))

    defer:
      val id = await(Randoms.nextString(8))
      val (_, result) = await(Fibers.parallel(blocker(id), reporter(id)))
      result


  def scenarioUrl(scenario: Int) = uri"http://localhost:8080/$scenario"

  def scenarios = Seq(scenario1, scenario2, scenario3, scenario4, scenario5, scenario6, scenario7, scenario8, scenario9, scenario10)
  //def scenarios: Seq[(Int => Uri) => String < Fibers] = Seq(scenario10)

  run:
    val scenariosReady: Seq[String < Fibers] = scenarios.map(s => s(scenarioUrl))

    defer:
      await(Fibers.parallel(scenariosReady))
