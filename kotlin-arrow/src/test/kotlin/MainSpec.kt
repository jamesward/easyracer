import io.kotest.core.extensions.install
import io.kotest.core.spec.style.FunSpec
import io.kotest.extensions.testcontainers.TestContainerExtension
import io.kotest.matchers.collections.shouldContainOnly
import org.testcontainers.images.PullPolicy

class MainSpec : FunSpec() {
    init {
        val container = install(TestContainerExtension("ghcr.io/jamesward/easyracer")) {
            withImagePullPolicy(PullPolicy.alwaysPull())
            withExposedPorts(8080)
        }

        test("should work") {
            fun url(i: Int) = "http://localhost:${container.firstMappedPort}/$i"
            results(::url).shouldContainOnly("right")
        }
    }
}