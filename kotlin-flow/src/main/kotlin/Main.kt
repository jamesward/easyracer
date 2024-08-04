@file:OptIn(ExperimentalCoroutinesApi::class, FlowPreview::class)

import com.sun.management.OperatingSystemMXBean
import io.ktor.client.*
import io.ktor.client.engine.cio.*
import io.ktor.client.request.*
import io.ktor.client.statement.*
import io.ktor.http.*
import kotlinx.coroutines.ExperimentalCoroutinesApi
import kotlinx.coroutines.FlowPreview
import kotlinx.coroutines.flow.*
import kotlinx.coroutines.time.delay
import java.lang.management.ManagementFactory
import java.security.MessageDigest
import java.time.Duration
import java.util.*
import kotlin.random.Random
import kotlin.time.Duration.Companion.seconds

val client = HttpClient(CIO) {
    followRedirects = false
    engine {
        endpoint {
            maxConnectionsCount = 10_000
            maxConnectionsPerRoute = 10_000
        }
    }
}

fun HttpClient.getAsFlow(urlString: String): Flow<HttpResponse> =
    flow { emit(this@getAsFlow.get(urlString)) }

// Note: Intentionally, only url handling code is shared across scenarios
suspend fun scenario1(url: (Int) -> String): String {
    val req = client.getAsFlow(url(1)).map { it.bodyAsText() }

    return merge(req, req).first()
}

suspend fun scenario2(url: (Int) -> String): String {
    val req = client.getAsFlow(url(2)).catch {}.map { it.bodyAsText() }

    return merge(req, req).first()
}

suspend fun scenario3(url: (Int) -> String): String {
    val req = client.getAsFlow(url(3)).map { it.bodyAsText() }

    return List(10_000) { req }
        // Uncomment the following on macOS
//        .mapIndexed { i, flow ->
//            { i }.asFlow().onEach { delay(Duration.ofMillis(it / 2L)) }.flatMapConcat { flow }
//        }
        .merge().first()
}

suspend fun scenario4(url: (Int) -> String): String {
    val req = client.getAsFlow(url(4)).map { it.bodyAsText() }

    return merge(req.timeout(1.seconds), req).first()
}

suspend fun scenario5(url: (Int) -> String): String {
    val req = client.getAsFlow(url(5)).filter { it.status.isSuccess() }.map { it.bodyAsText() }

    return merge(req, req).first()
}

suspend fun scenario6(url: (Int) -> String): String {
    val req = client.getAsFlow(url(6)).filter { it.status.isSuccess() }.map { it.bodyAsText() }

    return merge(req, req, req).first()
}

suspend fun scenario7(url: (Int) -> String): String {
    val req = client.getAsFlow(url(7)).filter { it.status.isSuccess() }.map { it.bodyAsText() }

    return merge(req.onStart { delay(Duration.ofSeconds(3)) }, req).first()
}

suspend fun scenario8(url: (Int) -> String): String {
    val open = client.getAsFlow(url(8) + "?open").filter { it.status.isSuccess() }.map { it.bodyAsText() }
    fun use(id: String) = client.getAsFlow(url(8) + "?use=$id")
    fun close(id: String) = client.getAsFlow(url(8) + "?close=$id")

    val reqRes = open.flatMapConcat { id ->
        use(id).flatMapConcat { result ->
            close(id)
                .filter { result.status.isSuccess() }
                .map { result.bodyAsText() }
        }
    }

    return merge(reqRes, reqRes).first()
}

suspend fun scenario9(url: (Int) -> String): String {
    val req = client.getAsFlow(url(9)).filter { it.status.isSuccess() }.map { it.bodyAsText() }

    return List(10) { req }.merge().reduce { l, r -> l + r }
}

suspend fun scenario10(url: (Int) -> String): String {
    val id = UUID.randomUUID().toString()

    val messageDigest = MessageDigest.getInstance("SHA-512")

    val blocking = flow { while (true) { emit(Unit) } }
        .runningFold(Random.nextBytes(512)) { bytes, _ ->
            messageDigest.digest(bytes)
        }
        .map { null }

    val blocker = merge(
        client.getAsFlow(url(10) + "?$id").map { it.bodyAsText() },
        blocking
    ).filterNotNull().take(1).map { null }

    fun reporter(): Flow<String> {
        val osBean = ManagementFactory.getPlatformMXBean(OperatingSystemMXBean::class.java)
        val load = osBean.processCpuLoad * osBean.availableProcessors
        return client.getAsFlow(url(10) + "?$id=$load").flatMapConcat {
            if (it.status.isSuccess()) {
                flow { emit(it.bodyAsText()) }
            } else if ((it.status.value >= 300) && (it.status.value < 400)) {
                delay(Duration.ofSeconds(1))
                reporter()
            } else {
                throw Exception(it.bodyAsText())
            }
        }
    }

    return merge(blocker, reporter()).filterNotNull().first()
}

val scenarios = listOf(
    ::scenario1,
    ::scenario2,
    ::scenario3,
    ::scenario4,
    ::scenario5,
    ::scenario6,
    ::scenario7,
    ::scenario8,
    ::scenario9,
    ::scenario10,
)

suspend fun results(url: (Int) -> String) = scenarios.map {
    it(url)
}

suspend fun main() {
    fun url(scenario: Int) = "http://localhost:8080/$scenario"
    println(results(::url))
}
