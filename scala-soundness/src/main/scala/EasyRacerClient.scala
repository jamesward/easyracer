import capricious.RandomSize
import com.sun.management.OperatingSystemMXBean
import soundness.{classOf as _, *}
import soundness.AsyncError.Reason.Cancelled
import soundness.asyncTermination.cancel
import soundness.executives.direct
import soundness.internetAccess.enabled
import soundness.logging.silent
import soundness.parameterInterpretation.posix
import soundness.stdioSources.virtualMachine.ansi
import soundness.threadModels.virtual
import soundness.unhandledErrors.stackTrace

import java.lang.management.ManagementFactory
import java.lang.System as JavaSystem
import java.security.MessageDigest
import scala.annotation.tailrec

def scenario1(scenarioUrl: Text => HttpUrl): Text raises ConnectError raises HttpError raises AsyncError =
  def req = scenarioUrl(t"1").fetch().receive[Text]
  supervise:
    Seq(async(req), async(req)).raceAndCancelLosers()

def scenario2(scenarioUrl: Text => HttpUrl): Text raises ConnectError raises HttpError raises AsyncError =
  def req = scenarioUrl(t"2").fetch().receive[Text]
  supervise:
    Seq(async(req), async(req)).raceAndCancelLosers()

def scenario3(scenarioUrl: Text => HttpUrl): Text raises ConnectError raises HttpError raises AsyncError =
  def req = scenarioUrl(t"3").fetch().receive[Text]
  supervise:
    Seq.fill(10_000)(async(req)).raceAndCancelLosers()

def scenario4(scenarioUrl: Text => HttpUrl): Text raises ConnectError raises HttpError raises AsyncError =
  def req = scenarioUrl(t"4").fetch().receive[Text]
  supervise:
    Seq(
      async(req),
      async:
        Seq(
          async(req),
          async:
            snooze(1*Second)
            t"1 second delay",
        ).raceAndCancelLosers()
    ).sequence.await().head
  // TODO Does not work:
//  supervise:
//    Seq(
//      async(req),
//      async:
//        async(req).await(1*Second),
//    ).raceAndCancelLosers()

def scenario5(scenarioUrl: Text => HttpUrl): Text raises ConnectError raises HttpError raises AsyncError =
  def req = scenarioUrl(t"5").fetch().receive[Text]
  supervise:
    Seq(async(req), async(req)).raceAndCancelLosers()

def scenario6(scenarioUrl: Text => HttpUrl): Text raises ConnectError raises HttpError raises AsyncError =
  def req = scenarioUrl(t"6").fetch().receive[Text]
  supervise:
    Seq(async(req), async(req), async(req)).raceAndCancelLosers()

def scenario7(scenarioUrl: Text => HttpUrl): Text raises ConnectError raises HttpError raises AsyncError =
  def req = scenarioUrl(t"7").fetch().receive[Text]
  supervise:
    Seq(
      async(req),
      async:
        snooze(4*Second)
        req,
    ).raceAndCancelLosers()

def scenario8(scenarioUrl: Text => HttpUrl): Text raises ConnectError raises HttpError raises AsyncError =
  def open = scenarioUrl(t"8").copy(query = t"open").fetch().receive[Text]
  def use(id: Text) = scenarioUrl(t"8").copy(query = t"use=$id").fetch().receive[Text]
  def close(id: Text) = scenarioUrl(t"8").copy(query = t"close=$id").fetch().receive[Text]

  def reqRes: Text =
    val id = open
    try use(id)
    finally close(id)

  supervise:
    Seq(async(reqRes), async(reqRes)).raceAndCancelLosers()

def scenario9(scenarioUrl: Text => HttpUrl): Text raises ConnectError raises HttpError raises AsyncError =
  // Explicitly specify type so that it doesn't fetch inferred as `Text`
  def req: Text raises ConnectError raises HttpError = scenarioUrl(t"9").fetch().receive[Text]
  supervise:
    Seq.fill(10):
      async:
        recover:
          case HttpError(_, _) => t""
        .within(req)
      .map(JavaSystem.nanoTime() -> _)
    .sequence
    .map(_.sortBy(_._1).map(_._2).reduce(_ + _))
    .await()

def scenario10(scenarioUrl: Text => HttpUrl): Text raises ConnectError raises HttpError raises AsyncError =
  val id = random[Int]()
  val messageDigest = MessageDigest.getInstance("SHA-512")

  def blocking: Text =
    given RandomSize = (_: Random) => 512
    @tailrec def digest(bytes: Array[Byte]): Text raises AsyncError =
      // TODO Does not work
      // relent()
      // digest(messageDigest.digest(bytes))
      if Thread.interrupted() then abort(AsyncError(Cancelled))
      else digest(messageDigest.digest(bytes))

    digest(IArray.genericWrapArray(random[IArray[Byte]]()).toArray)

  def blocker(using Monitor) =
    Seq(
      async:
        scenarioUrl(t"10").copy(query = t"$id").fetch().receive[Text],
      async:
        blocking
    ).raceAndCancelLosers()

  @tailrec def reporter(using Monitor): Text =
    val osBean = ManagementFactory.getPlatformMXBean(classOf[OperatingSystemMXBean])
    val load = osBean.getProcessCpuLoad * osBean.getAvailableProcessors
    val resp = scenarioUrl(t"10").copy(query = t"$id=${load.toString}").fetch()
    if resp.status == Http.Found then
      snooze(1*Second)
      reporter
    else
      resp.receive[Text]

  supervise:
    val result = async(reporter)
    async(blocker).await()
    result.await()

def scenario11(scenarioUrl: Text => HttpUrl): Text raises ConnectError raises HttpError raises AsyncError =
  def req = scenarioUrl(t"11").fetch().receive[Text]
  supervise:
    Seq(
      async(Seq(async(req), async(req)).raceAndCancelLosers()),
      async(req),
    ).raceAndCancelLosers()

val scenarios: Seq[(Text => HttpUrl) => Text raises ConnectError raises HttpError raises AsyncError] = Seq(
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
  scenario11,
)
@main def runAllScenarios(): Unit = application(Nil):
  def scenarioUrl(scenario: Text): HttpUrl = url"http://localhost:8080/$scenario"

  scenarios
    .map: scenario =>
      recover:
        case ConnectError(reason) => t"wrong: connect error $reason"
        case HttpError(status, _) => t"wrong: HTTP error ${status.code}"
        case AsyncError(reason)   => t"wrong: async error $reason"
      .within(scenario(scenarioUrl))
    .foreach:
      Out.println(_)
  Exit.Ok
