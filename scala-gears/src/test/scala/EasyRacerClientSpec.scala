import com.dimafeng.testcontainers.GenericContainer
import gears.async.*
import gears.async.default.given
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.testcontainers.containers.wait.strategy.Wait
import org.testcontainers.images.PullPolicy

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

  def scenarioUrl(scenario: Int) = s"http://localhost:$port/$scenario"

  EasyRacerClient.scenarios.zipWithIndex.foreach: (fn, number) =>
    s"scenario ${number + 1}" should "work" in:
      Async.blocking:
        fn(scenarioUrl) shouldBe "right"
