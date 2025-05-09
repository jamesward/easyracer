import com.dimafeng.testcontainers.GenericContainer
import kyo.*
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import org.testcontainers.containers.wait.strategy.Wait
import org.testcontainers.images.PullPolicy
import sttp.client3.UriContext

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

  def scenarioUrl(scenario: Int) = uri"http://localhost:$port/$scenario"

  inline given Flat[FailedRequest | Timeout] = Flat.unsafe.bypass // won't be needed in next release

  EasyRacerClient.scenarios.zipWithIndex.foreach: (fn, number) =>
    s"scenario ${number + 1}" should "work" in:
      import AllowUnsafe.embrace.danger
      KyoApp.Unsafe.runAndBlock(1.minute)(fn(scenarioUrl)).getOrThrow.shouldBe("right")
