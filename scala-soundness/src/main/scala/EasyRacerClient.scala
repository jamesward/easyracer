import capricious.RandomSize
import com.sun.management.OperatingSystemMXBean
import parasite.ConcurrencyError.Reason.Timeout
import soundness.*
import soundness.executives.direct
import soundness.internetAccess.enabled
import soundness.logging.silent
import soundness.orphanDisposal.cancel
import soundness.parameterInterpretation.posix
import soundness.stdioSources.virtualMachine.ansi
import soundness.threadModels.virtual
import soundness.unhandledErrors.stackTrace

import java.lang.management.ManagementFactory
import java.security.MessageDigest

def scenario1(scenarioUrl: Text => HttpUrl): Text raises HttpError raises ConcurrencyError =
  def req = scenarioUrl(t"1").get().as[Text]
  // TODO:
  // Loser HTTP request isn't cancelled, figure out why and fix 
  supervise:
    Seq(async(req), async(req)).race()

def scenario2(scenarioUrl: Text => HttpUrl): Text raises HttpError raises ConcurrencyError =
  def req = scenarioUrl(t"2").get().as[Text]
  supervise:
    Seq(async(req), async(req)).race()

def scenario3(scenarioUrl: Text => HttpUrl): Text raises HttpError raises ConcurrencyError =
  def req = scenarioUrl(t"3").get().as[Text]
  supervise:
    Seq.fill(10_000)(async(req)).race()

def scenario4(scenarioUrl: Text => HttpUrl): Text raises HttpError raises ConcurrencyError =
  def req = scenarioUrl(t"4").get().as[Text]
  supervise:
    Seq(
      async(req),
      async:
        Seq(
          async(req),
          async:
            sleep(1*Second)
            t"1 second delay",
        ).race()
    ).sequence.await().head
  // TODO Does not work:
//  supervise:
//    Seq(
//      async(req),
//      async:
//        async(req).await(1*Second),
//    ).race()

def scenario5(scenarioUrl: Text => HttpUrl): Text raises HttpError raises ConcurrencyError =
  def req = scenarioUrl(t"5").get().as[Text]
  supervise:
    Seq(async(req), async(req)).race()

def scenario6(scenarioUrl: Text => HttpUrl): Text raises HttpError raises ConcurrencyError =
  def req = scenarioUrl(t"6").get().as[Text]
  supervise:
    Seq(async(req), async(req), async(req)).race()

def scenario7(scenarioUrl: Text => HttpUrl): Text raises HttpError raises ConcurrencyError =
  def req = scenarioUrl(t"7").get().as[Text]
  // TODO:
  // Loser HTTP request isn't cancelled, figure out why and fix 
  supervise:
    Seq(
      async(req),
      async:
        sleep(4*Second)
        req,
    ).race()

def scenario8(scenarioUrl: Text => HttpUrl): Text raises HttpError raises ConcurrencyError =
  def open = scenarioUrl(t"8").copy(query = t"open").get().as[Text]
  def use(id: Text) = scenarioUrl(t"8").copy(query = t"use=$id").get().as[Text]
  def close(id: Text) = scenarioUrl(t"8").copy(query = t"close=$id").get().as[Text]

  def reqRes: Text =
    val id = open
    try use(id)
    finally close(id)

  supervise:
    Seq(async(reqRes), async(reqRes)).race()

def scenario9(scenarioUrl: Text => HttpUrl): Text raises ConcurrencyError =
  // Explicitly specify type so that it doesn't get inferred as `Text`
  def req: Text raises HttpError = scenarioUrl(t"9").get().as[Text]
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

def scenario10(scenarioUrl: Text => HttpUrl): Text raises HttpError raises ConcurrencyError =
  val id = random[Int]()
  val messageDigest = MessageDigest.getInstance("SHA-512")

  def blocking: Text =
    given RandomSize = (_: Random) => 512
    @tailrec def digest(bytes: Array[Byte]): Text raises ConcurrencyError =
      // TODO
      // Per parasite README, this is supposedly how you check for cancellation:
      // https://github.com/propensive/parasite?tab=readme-ov-file#cancelation
      // But it doesn't appear to be implemented yet
      // acquiesce() // Does not compile
      if Thread.interrupted() then abort(ConcurrencyError(Timeout))
      else digest(messageDigest.digest(bytes))

    digest(IArray.genericWrapArray(random[IArray[Byte]]()).toArray)

  def blocker(using Monitor) =
    Seq(
      async:
        scenarioUrl(t"10").copy(query = t"$id").get().as[Text],
      async:
        blocking
    ).race()

  @tailrec def reporter(using Monitor): Text =
    val osBean = ManagementFactory.getPlatformMXBean(classOf[OperatingSystemMXBean])
    val load = osBean.getProcessCpuLoad * osBean.getAvailableProcessors
    val resp = scenarioUrl(t"10").copy(query = t"$id=${load.toString}").get()
    if resp.status == HttpStatus.Found then
      sleep(1*Second)
      reporter
    else
      resp.as[Text]

  supervise:
    val result = async(reporter)
    async(blocker).await()
    result.await()

def scenario11(scenarioUrl: Text => HttpUrl): Text raises HttpError raises ConcurrencyError =
  def req = scenarioUrl(t"11").get().as[Text]
  supervise:
    Seq(
      async(Seq(async(req), async(req)).race()),
      async(req),
    ).race()

val scenarios: Seq[(Text => HttpUrl) => Text raises HttpError raises ConcurrencyError] = Seq(
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
        case ConcurrencyError(reason) => t"wrong: concurrency error $reason"
      .within(scenario(scenarioUrl))
    .foreach:
      Out.println(_)
  Exit.Ok
