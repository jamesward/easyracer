import com.sun.management.OperatingSystemMXBean
import io.ktor.client.*
import io.ktor.client.plugins.*
import io.ktor.client.request.*
import io.ktor.client.statement.*
import io.ktor.http.*
import kotlinx.coroutines.*
import kotlinx.coroutines.selects.select
import kotlinx.coroutines.time.delay
import java.lang.management.ManagementFactory
import java.security.MessageDigest
import java.time.Duration
import java.time.Instant
import java.util.UUID
import kotlin.random.Random

val client = HttpClient {
    install(HttpTimeout)
    followRedirects = false
}

// Note: Intentionally, only url handling code is shared across scenarios
suspend fun scenario1(url: (Int) -> String): String = coroutineScope {
    try {
        select {
            async { client.get(url(1)) }.onAwait { it }
            async { client.get(url(1)) }.onAwait { it }
        }.bodyAsText()
    } finally {
        coroutineContext.cancelChildren()
    }
}

suspend fun scenario2(url: (Int) -> String): String = coroutineScope {
    try {
        select {
            async {
                try {
                    client.get(url(2))
                } catch (_: Exception) {
                    awaitCancellation()
                }
            }.onAwait { it }
            async {
                try {
                    client.get(url(2))
                } catch (_: Exception) {
                    awaitCancellation()
                }
            }.onAwait { it }
        }.bodyAsText()
    } finally {
        coroutineContext.cancelChildren()
    }
}

suspend fun scenario3(url: (Int) -> String): String = coroutineScope {
    try {
        select {
            List(10_000) {
                // On certain (macOS?) machines, creating 100+ concurrent connections at a time
                // results in connections being dropped due to "Connection reset by peer" error.
                //
                // If you are running on such a machine, uncomment the following line:
//                Thread.sleep(0, 500_000)
                async { client.get(url(3)) }.onAwait { it }
            }
        }.bodyAsText()
    } finally {
        coroutineContext.cancelChildren()
    }
}

suspend fun scenario4(url: (Int) -> String): String = coroutineScope {
    try {
        select {
            async {
                try {
                    client.get(url(4)) {
                        timeout {
                            requestTimeoutMillis = 1000
                        }
                    }
                } catch (_: Exception) {
                    awaitCancellation()
                }
            }.onAwait { it }
            async { client.get(url(4)) }.onAwait { it }
        }.bodyAsText()
    } finally {
        coroutineContext.cancelChildren()
    }
}

suspend fun scenario5(url: (Int) -> String): String = coroutineScope {
    suspend fun req(): HttpResponse =
        client.get(url(5)).let {
            if (it.status.isSuccess()) it
            else awaitCancellation()
        }
    try {
        select {
            async { req() }.onAwait { it }
            async { req() }.onAwait { it }
        }.bodyAsText()
    } finally {
        coroutineContext.cancelChildren()
    }
}

suspend fun scenario6(url: (Int) -> String): String = coroutineScope {
    suspend fun req(): HttpResponse =
        client.get(url(6)).let {
            if (it.status.isSuccess()) it
            else awaitCancellation()
        }
    try {
        select {
            async { req() }.onAwait { it }
            async { req() }.onAwait { it }
            async { req() }.onAwait { it }
        }.bodyAsText()
    } finally {
        coroutineContext.cancelChildren()
    }
}

suspend fun scenario7(url: (Int) -> String): String = coroutineScope {
    try {
        select {
            async { client.get(url(7)) }.onAwait { it }
            async {
                delay(Duration.ofSeconds(3))
                client.get(url(7))
            }.onAwait { it }
        }.bodyAsText()
    } finally {
        coroutineContext.cancelChildren()
    }
}

suspend fun scenario8(url: (Int) -> String): String = coroutineScope {
    suspend fun req(): String {
        val id = client.get(url(8) + "?open").bodyAsText()
        return client.get(url(8) + "?use=$id").let {
            client.get(url(8) + "?close=$id").bodyAsText()

            if (it.status.isSuccess()) it.bodyAsText()
            else awaitCancellation()
        }
    }

    try {
        select {
            async { req() }.onAwait { it }
            async { req() }.onAwait { it }
        }
    } finally {
        coroutineContext.cancelChildren()
    }
}

suspend fun scenario9(url: (Int) -> String): String = coroutineScope {
    val letters = List(10) {
        async {
            client.get(url(9)).let {
                if (it.status.isSuccess())
                    Instant.now() to it.bodyAsText()
                else null
            }
        }
    }.awaitAll()

    letters.filterNotNull().sortedBy { it.first }.joinToString("") { it.second }
}

suspend fun scenario10(url: (Int) -> String): String = coroutineScope {
    val id = UUID.randomUUID().toString()

    val messageDigest = MessageDigest.getInstance("SHA-512")

    suspend fun blocking() = coroutineScope {
        var result = Random.nextBytes(512)
        while (isActive) {
            result = messageDigest.digest(result)
        }
    }

    suspend fun blocker(): Unit = coroutineScope {
        try {
            select {
                async { client.get(url(10) + "?$id") }.onAwait { }
                async(Dispatchers.IO) { blocking() }.onAwait { }
            }
        } finally {
            coroutineContext.cancelChildren()
        }
    }

    suspend fun reporter(): String = coroutineScope {
        val osBean = ManagementFactory.getPlatformMXBean(OperatingSystemMXBean::class.java)
        val load = osBean.processCpuLoad * osBean.availableProcessors
        val resp = client.get(url(10) + "?$id=$load")
        if (resp.status.isSuccess()) {
            resp.bodyAsText()
        }
        else if ((resp.status.value >= 300) && (resp.status.value < 400)) {
            delay(1000)
            reporter()
        }
        else {
            throw Exception(resp.bodyAsText())
        }
    }

    launch { blocker() }
    reporter()
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
    coroutineScope {
        async { it(url) }
    }
}.awaitAll()

suspend fun main() {
    fun url(scenario: Int) = "http://localhost:8080/$scenario"
    println(results(::url))
}
