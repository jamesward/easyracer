import capricious.RandomSize
import com.sun.management.OperatingSystemMXBean
import soundness.*
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
import java.security.MessageDigest

def scenario1(scenarioUrl: Text => HttpUrl): Text raises HttpError raises AsyncError =
  def req = scenarioUrl(t"1").fetch().as[Text]
  supervise:
    Seq(async(req), async(req)).raceAndCancelLosers()

def scenario2(scenarioUrl: Text => HttpUrl): Text raises HttpError raises AsyncError =
  def req = scenarioUrl(t"2").fetch().as[Text]
  supervise:
    Seq(async(req), async(req)).raceAndCancelLosers()

def scenario3(scenarioUrl: Text => HttpUrl): Text raises HttpError raises AsyncError =
  def req = scenarioUrl(t"3").fetch().as[Text]
  supervise:
    Seq.fill(10_000)(async(req)).raceAndCancelLosers()

def scenario4(scenarioUrl: Text => HttpUrl): Text raises HttpError raises AsyncError =
  def req = scenarioUrl(t"4").fetch().as[Text]
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

def scenario5(scenarioUrl: Text => HttpUrl): Text raises HttpError raises AsyncError =
  def req = scenarioUrl(t"5").fetch().as[Text]
  supervise:
    Seq(async(req), async(req)).raceAndCancelLosers()

def scenario6(scenarioUrl: Text => HttpUrl): Text raises HttpError raises AsyncError =
  def req = scenarioUrl(t"6").fetch().as[Text]
  supervise:
    Seq(async(req), async(req), async(req)).raceAndCancelLosers()

def scenario7(scenarioUrl: Text => HttpUrl): Text raises HttpError raises AsyncError =
  def req = scenarioUrl(t"7").fetch().as[Text]
  supervise:
    Seq(
      async(req),
      async:
        snooze(4*Second)
        req,
    ).raceAndCancelLosers()

def scenario8(scenarioUrl: Text => HttpUrl): Text raises HttpError raises AsyncError =
  def open = scenarioUrl(t"8").copy(query = t"open").fetch().as[Text]
  def use(id: Text) = scenarioUrl(t"8").copy(query = t"use=$id").fetch().as[Text]
  def close(id: Text) = scenarioUrl(t"8").copy(query = t"close=$id").fetch().as[Text]

  def reqRes: Text =
    val id = open
    try use(id)
    finally close(id)

  supervise:
    Seq(async(reqRes), async(reqRes)).raceAndCancelLosers()

def scenario9(scenarioUrl: Text => HttpUrl): Text raises HttpError raises AsyncError =
  // Explicitly specify type so that it doesn't get inferred as `Text`
  def req: Text raises HttpError = scenarioUrl(t"9").fetch().as[Text]
  supervise:
    Seq.fill(10):
      async:
        mend:
          case HttpError(_, _) => t""
        .within(req)
      .map(System.nanoTime() -> _)
    .sequence
    .map(_.sortBy(_._1).map(_._2).reduce(_ + _))
    .await()

def scenario10(scenarioUrl: Text => HttpUrl): Text raises HttpError raises AsyncError =
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
        scenarioUrl(t"10").copy(query = t"$id").fetch().as[Text],
      async:
        blocking
    ).raceAndCancelLosers()

  @tailrec def reporter(using Monitor): Text =
    val osBean = ManagementFactory.getPlatformMXBean(classOf[OperatingSystemMXBean])
    val load = osBean.getProcessCpuLoad * osBean.getAvailableProcessors
    val resp = scenarioUrl(t"10").copy(query = t"$id=${load.toString}").fetch()
    if resp.status == HttpStatus.Found then
      snooze(1*Second)
      reporter
    else
      resp.as[Text]

  supervise:
    val result = async(reporter)
    async(blocker).await()
    result.await()

def scenario11(scenarioUrl: Text => HttpUrl): Text raises HttpError raises AsyncError =
  def req = scenarioUrl(t"11").fetch().as[Text]
  supervise:
    Seq(
      async(Seq(async(req), async(req)).raceAndCancelLosers()),
      async(req),
    ).raceAndCancelLosers()

val scenarios: Seq[(Text => HttpUrl) => Text raises HttpError raises AsyncError] = Seq(
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
      mend:
        case HttpError(status, _)     => t"wrong: HTTP error ${status.code}"
        case AsyncError(reason) => t"wrong: concurrency error $reason"
      .within(scenario(scenarioUrl))
    .foreach:
      Out.println(_)
  Exit.Ok
