import io.kotest.core.extensions.install
import io.kotest.core.spec.style.FunSpec
import io.kotest.extensions.testcontainers.ContainerExtension
import io.kotest.matchers.collections.shouldContainOnly
import org.slf4j.LoggerFactory
import org.testcontainers.containers.GenericContainer
import org.testcontainers.containers.output.Slf4jLogConsumer
import org.testcontainers.containers.wait.strategy.Wait
import org.testcontainers.images.PullPolicy

class MainSpec : FunSpec() {
    init {
        val logger = LoggerFactory.getLogger(this::class.java)
        val logConsumer = Slf4jLogConsumer(logger)

        val container = GenericContainer("ghcr.io/jamesward/easyracer")
            .withExposedPorts(8080)
            .withImagePullPolicy(PullPolicy.alwaysPull())
            .withCommand("--debug")
            .withLogConsumer(logConsumer)
            .waitingFor(Wait.forHttp("/"))

        val server = install(ContainerExtension(container))

        test("should work") {
            fun url(i: Int) = "http://localhost:${server.firstMappedPort}/$i"
            results(::url).shouldContainOnly("right")
        }
    }
}
