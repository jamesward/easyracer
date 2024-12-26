import com.dimafeng.testcontainers.GenericContainer
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.testcontainers.containers.wait.strategy.Wait
import org.testcontainers.images.PullPolicy
import soundness.*

import scala.compiletime.uninitialized

class EasyRacerClientSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll:

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

  def scenarioUrl(scenario: Text) = url"http://localhost:$port/$scenario"

  scenarios.zipWithIndex.foreach: (fn, number) =>
    s"scenario ${number + 1}" should "work" in:
      mend:
        case t: Exception => fail(t)
      .within:
        fn(scenarioUrl).toString shouldBe "right"
