import org.testcontainers.containers.wait.strategy.Wait
import org.testcontainers.images.PullPolicy

import com.dimafeng.testcontainers.GenericContainer

class EasyRacerContainer() extends GenericContainer(
  "ghcr.io/jamesward/easyracer",
  Seq(8080),
  waitStrategy = Option(Wait.forHttp("/")),
  imagePullPolicy = Option(PullPolicy.alwaysPull())
)

object EasyRacerContainer {
  def apply() = new EasyRacerContainer()
}
