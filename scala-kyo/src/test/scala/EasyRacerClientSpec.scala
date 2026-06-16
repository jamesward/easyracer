import com.dimafeng.testcontainers.GenericContainer
import kyo.*
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import org.testcontainers.containers.wait.strategy.Wait
import org.testcontainers.images.PullPolicy

import scala.compiletime.uninitialized
import scala.concurrent.{ExecutionContext, Future}

class EasyRacerClientSpec extends AsyncFlatSpec with Matchers with BeforeAndAfterAll:

  implicit override def executionContext: ExecutionContext = ExecutionContext.global

  var container: GenericContainer = uninitialized
  var port: Int = 0

  override protected def beforeAll(): Unit =
    super.beforeAll()
    container = GenericContainer(
      "ghcr.io/jamesward/easyracer",
      Seq(8080),
      waitStrategy = Wait.forHttp("/"),
      imagePullPolicy = PullPolicy.alwaysPull()
    )
    container.start()
    port = container.container.getFirstMappedPort

  override protected def afterAll(): Unit =
    container.stop()
    super.afterAll()

  def scenarioUrl(scenario: Int) = s"http://localhost:$port/$scenario"

  EasyRacerClient.scenarios.zipWithIndex.foreach: (fn, number) =>
    s"scenario ${number + 1}" should "work" in:
      import AllowUnsafe.embrace.danger
      // Each scenario gets its own HttpClient with a pool large enough for scenario 3's 10k racers,
      // and a configuration that disables the 5s default timeout and auto-redirects (needed for
      // scenarios 7 and 10).
      val effect =
        HttpClient.init(maxConnectionsPerHost = 10_100).map: client =>
          HttpClient.let(client):
            HttpClient.withConfig(_.timeout(Duration.Infinity).followRedirects(false)):
              fn(scenarioUrl)
      KyoApp.Unsafe.runAndBlock(2.minutes)(effect).getOrThrow.shouldBe("right")
