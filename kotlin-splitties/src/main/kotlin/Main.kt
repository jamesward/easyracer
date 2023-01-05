@file:OptIn(ExperimentalSplittiesApi::class)

import io.ktor.client.*
import io.ktor.client.call.*
import io.ktor.client.plugins.*
import io.ktor.client.request.*
import io.ktor.client.statement.*
import io.ktor.client.utils.*
import io.ktor.http.*
import io.ktor.util.*
import io.ktor.util.date.*
import kotlinx.coroutines.async
import kotlinx.coroutines.awaitAll
import kotlinx.coroutines.awaitCancellation
import kotlinx.coroutines.coroutineScope
import kotlinx.coroutines.time.delay
import splitties.coroutines.launchRacer
import splitties.coroutines.race
import splitties.coroutines.raceOf
import splitties.experimental.ExperimentalSplittiesApi
import java.time.Duration
import java.time.Instant


val client = HttpClient {
    install(HttpTimeout)
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


val scenarios = listOf(::scenario1, ::scenario2, ::scenario3, ::scenario4, ::scenario5, ::scenario6, ::scenario7, ::scenario8, ::scenario9)
//val scenarios = listOf(::scenario8)

suspend fun results(url: (Int) -> String) = scenarios.map {
    coroutineScope {
        async { it(url) }
    }
}.awaitAll()

suspend fun main() {
    fun url(scenario: Int) = "http://localhost:8080/$scenario"
    println(results(::url))
}