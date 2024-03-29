import com.dimafeng.testcontainers.GenericContainer
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.testcontainers.containers.wait.strategy.Wait
import org.testcontainers.images.PullPolicy
import sttp.client3.UriContext

class EasyRacerClientSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll:

  val scenarios = List(
    1 -> EasyRacerClient.scenario1,
    2 -> EasyRacerClient.scenario2,
    3 -> EasyRacerClient.scenario3,
    4 -> EasyRacerClient.scenario4,
    5 -> EasyRacerClient.scenario5,
    6 -> EasyRacerClient.scenario6,
    7 -> EasyRacerClient.scenario7,
    8 -> EasyRacerClient.scenario8,
    9 -> EasyRacerClient.scenario9,
    10 -> EasyRacerClient.scenario10,
  )

  var container: GenericContainer = _
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

  scenarios.foreach { (number, fn) =>
    s"scenario $number" should "work" in {
      fn(scenarioUrl) shouldBe "right"
    }
  }
