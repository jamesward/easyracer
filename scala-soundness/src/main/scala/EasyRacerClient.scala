import contingency.strategies.throwUnsafely
import soundness.*
import soundness.executives.direct
import soundness.internetAccess.enabled
import soundness.logging.silent
import soundness.orphanDisposal.cancel
import soundness.parameterInterpretation.posix
import soundness.stdioSources.virtualMachine.ansi
import soundness.threadModels.virtual
import soundness.unhandledErrors.silent

def scenario1(scenarioUrl: Text => HttpUrl): Text =
  val url = scenarioUrl(t"1")
  supervise:
    Seq(
      async:
        url.get().as[Text],
      async:
        url.get().as[Text],
    ).race()

def scenario2(scenarioUrl: Text => HttpUrl): Text =
  val url = scenarioUrl(t"2")
  supervise:
    Seq(
      async:
        url.get().as[Text],
      async:
        url.get().as[Text],
    ).race()

def scenario3(scenarioUrl: Text => HttpUrl): Text =
  val url = scenarioUrl(t"3")
  supervise:
    Seq.fill(10_000):
      async:
        url.get().as[Text]
    .race()

def scenario4(scenarioUrl: Text => HttpUrl): Text =
  val url = scenarioUrl(t"4")
  supervise:
    Seq(
      async:
        url.get().as[Text],
      async:
        Seq(
          async:
            url.get().as[Text],
          async:
            sleep(1*Second)
            t"wrong",
        ).race()
    ).race()

def scenario5(scenarioUrl: Text => HttpUrl): Text =
  val url = scenarioUrl(t"2")
  supervise:
    Seq(
      async:
        url.get().as[Text],
      async:
        url.get().as[Text],
    ).race()

def scenario6(scenarioUrl: Text => HttpUrl): Text =
  val url = scenarioUrl(t"2")
  supervise:
    Seq(
      async:
        url.get().as[Text],
      async:
        url.get().as[Text],
      async:
        url.get().as[Text],
    ).race()

def scenario7(scenarioUrl: Text => HttpUrl): Text =
  val url = scenarioUrl(t"7")
  supervise:
    Seq(
      async:
        url.get().as[Text],
      async:
        sleep(4*Second)
        url.get().as[Text],
    ).race()

def scenario9(scenarioUrl: Text => HttpUrl): Text =
  val url = scenarioUrl(t"9")
  supervise:
    Seq.fill(10):
      async:
        try url.get().as[Text]
        catch case _ => t""
      .map(System.nanoTime() -> _)
    .sequence
    .map(_.sortBy(_._1).map(_._2).reduce(_ + _))
    .await()

def scenario11(scenarioUrl: Text => HttpUrl): Text =
  val url = scenarioUrl(t"11")
  supervise:
    Seq(
      async:
        Seq(
          async:
            url.get().as[Text],
          async:
            url.get().as[Text],
        ).race(),
      async:
        url.get().as[Text],
    ).race()

val scenarios: Seq[(Text => HttpUrl) => Text] = Seq(
  scenario1,
  scenario2,
  scenario3,
  scenario4,
  scenario5,
  scenario6,
  scenario7,
  scenario9,
  scenario11,
)
@main def runAllScenarios(): Unit = application(Nil):
  def scenarioUrl(scenario: Text): HttpUrl = url"http://localhost:8080/$scenario"

  scenarios
    .map:
      _(scenarioUrl)
    .foreach:
      Out.println(_)
  Exit.Ok