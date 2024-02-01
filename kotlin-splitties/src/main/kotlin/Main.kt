@file:OptIn(ExperimentalSplittiesApi::class)

import com.sun.management.OperatingSystemMXBean
import io.ktor.client.*
import io.ktor.client.plugins.*
import io.ktor.client.request.*
import io.ktor.client.statement.*
import io.ktor.http.*
import kotlinx.coroutines.*
import kotlinx.coroutines.time.delay
import splitties.coroutines.launchRacer
import splitties.coroutines.race
import splitties.coroutines.raceOf
import splitties.coroutines.repeatWhileActive
import splitties.experimental.ExperimentalSplittiesApi
import java.lang.management.ManagementFactory
import java.security.MessageDigest
import java.time.Duration
import java.time.Instant
import java.util.*
import kotlin.random.Random


val client = HttpClient {
    install(HttpTimeout)
    followRedirects = false
}


suspend fun scenario1(url: (Int) -> String) =
    raceOf({
        client.get(url(1))
    }, {
        client.get(url(1))
    }).bodyAsText()


suspend fun scenario2(url: (Int) -> String) =
    raceOf({
        try {
            client.get(url(2))
        } catch (e: Exception) {
            awaitCancellation()
        }
    }, {
        try {
            client.get(url(2))
        } catch (e: Exception) {
            awaitCancellation()
        }
    }).bodyAsText()


suspend fun scenario3(url: (Int) -> String) = coroutineScope {
    race {
        repeat(10_000) {
            launchRacer {
                client.get(url(3))
            }
        }
    }.bodyAsText()
}


suspend fun scenario4(url: (Int) -> String) =
    raceOf({
        // todo: withTimeout(Duration.ofSeconds(1)) {
        try {
            client.get(url(4)) {
                timeout {
                    requestTimeoutMillis = 1000
                }
            }.bodyAsText()
        } catch (e: Exception) {
            awaitCancellation()
        }
    }, {
        client.get(url(4)).bodyAsText()
    })


suspend fun scenario5(url: (Int) -> String): String {
    suspend fun req(): String {
        try {
            val resp = client.get(url(5))
            require(resp.status.isSuccess())
            return resp.bodyAsText()
        } catch (e: IllegalArgumentException) {
            awaitCancellation()
        }
    }

    return raceOf({
        req()
    }, {
        req()
    })
}


suspend fun scenario6(url: (Int) -> String): String {
    suspend fun req(): String {
        try {
            val resp = client.get(url(6))
            require(resp.status.isSuccess())
            return resp.bodyAsText()
        } catch (e: IllegalArgumentException) {
            awaitCancellation()
        }
    }

    return raceOf({
        req()
    }, {
        req()
    }, {
        req()
    })
}


suspend fun scenario7(url: (Int) -> String) =
    raceOf({
        client.get(url(7))
    }, {
        delay(Duration.ofSeconds(3))
        client.get(url(7))
    }).bodyAsText()


suspend fun scenario8(url: (Int) -> String): String {
    suspend fun req(): String {
        val id = client.get(url(8) + "?open").bodyAsText()
        try {
            val resp = client.get(url(8) + "?use=$id")
            require(resp.status.isSuccess())
            return resp.bodyAsText()
        } finally {
            client.get(url(8) + "?close=$id").bodyAsText()
        }
    }

    return raceOf({
        try {
            req()
        } catch (e: Exception) {
            awaitCancellation()
        }
    }, {
        try {
            req()
        } catch (e: Exception) {
            awaitCancellation()
        }
    })
}

suspend fun scenario9(url: (Int) -> String): String {
    suspend fun req(): Pair<Instant, String>? {
        val resp = client.get(url(9))
        return if (resp.status.isSuccess()) {
            Instant.now() to resp.bodyAsText()
        } else {
            null
        }
    }

    return coroutineScope {
        val letters = List(10) {
            async {
                req()
            }
        }.awaitAll()

        letters.filterNotNull().sortedBy { it.first }.joinToString("") { it.second }
    }
}


suspend fun scenario10(url: (Int) -> String): String = coroutineScope {
    val id = UUID.randomUUID().toString()

    val messageDigest = MessageDigest.getInstance("SHA-512")

    suspend fun blocking() {
        var result = Random.nextBytes(512)
        repeatWhileActive {
            result = messageDigest.digest(result)
        }
    }

    suspend fun blocker(): Unit = raceOf({
        client.get(url(10) + "?$id").bodyAsText()
    }, {
        async(Dispatchers.IO) { blocking() }.await()
    })

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
//val scenarios = listOf(::scenario10)

suspend fun results(url: (Int) -> String) = scenarios.map {
    coroutineScope {
        async { it(url) }
    }
}.awaitAll()

suspend fun main() {
    fun url(scenario: Int) = "http://localhost:8080/$scenario"
    println(results(::url))
}
