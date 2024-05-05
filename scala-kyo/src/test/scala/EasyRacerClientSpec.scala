import com.dimafeng.testcontainers.GenericContainer
import kyo.*
import kyo.test.*
import zio.ZIO
import zio.test.*
import org.testcontainers.containers.wait.strategy.Wait

import org.testcontainers.images.PullPolicy
import sttp.client3.UriContext

import scala.compiletime.uninitialized
import scala.concurrent.{ExecutionContext, Future}

object EasyRacerClientSpec extends KyoSpecDefault:

  var container: GenericContainer = uninitialized
  var port: Int = 0
  def scenarioUrl(scenario: Int) = uri"http://localhost:$port/$scenario"

  def spec =
    suite("KyoEasyRacer") {
      EasyRacerClient.scenarios.zipWithIndex.map: (fn, number) =>
        test(s"scenario ${number + 1}") {
          fn(scenarioUrl).map(result => assertTrue(result == "right"))
        }
    } @@ TestAspect.sequential @@
      TestAspect.before(ZIO.attempt{
        container = GenericContainer(
          "ghcr.io/jamesward/easyracer",
          Seq(8080),
          waitStrategy = Wait.forHttp("/"),
          imagePullPolicy = PullPolicy.alwaysPull()
        )
        container.start()
        port = container.container.getFirstMappedPort
      }) @@
      TestAspect.after(ZIO.attempt(container.stop()))
