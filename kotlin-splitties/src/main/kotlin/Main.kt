@file:OptIn(ExperimentalSplittiesApi::class)

import io.ktor.client.*
import io.ktor.client.call.*
import io.ktor.client.request.*
import io.ktor.client.statement.*
import io.ktor.client.utils.*
import io.ktor.http.*
import io.ktor.util.*
import io.ktor.util.date.*
import kotlinx.coroutines.TimeoutCancellationException
import kotlinx.coroutines.async
import kotlinx.coroutines.awaitAll
import kotlinx.coroutines.awaitCancellation
import kotlinx.coroutines.coroutineScope
import kotlinx.coroutines.time.delay
import kotlinx.coroutines.time.withTimeout
import splitties.coroutines.launchRacer
import splitties.coroutines.race
import splitties.coroutines.raceOf
import splitties.experimental.ExperimentalSplittiesApi
import java.lang.IllegalArgumentException
import java.time.Duration


val client = HttpClient()


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
        }
        catch (e: Exception) {
            awaitCancellation()
        }
    }, {
        try {
            client.get(url(2))
        }
        catch (e: Exception) {
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


// todo: not working
suspend fun scenario4(url: (Int) -> String) =
    try {
        withTimeout(Duration.ofSeconds(5)) { // temporary while broken
            raceOf({
                try {
                    withTimeout(Duration.ofSeconds(1)) {
                        client.get(url(4)).bodyAsText()
                    }
                } catch (e: TimeoutCancellationException) {
                    awaitCancellation()
                }
            }, {
                client.get(url(4)).bodyAsText()
            })
        }
    }
    catch (e: TimeoutCancellationException) {
        "wrong"
    }


suspend fun scenario5(url: (Int) -> String): String {
    suspend fun req(): String {
        try {
            val resp = client.get(url(5))
            require(resp.status.isSuccess())
            return resp.bodyAsText()
        }
        catch (e: IllegalArgumentException) {
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
        }
        catch (e: IllegalArgumentException) {
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


// todo: not working
suspend fun scenario8(url: (Int) -> String): String {
    suspend fun req(): String {
        val id = client.get(url(8) + "?open").bodyAsText()
        try {
            val resp = client.get(url(8) + "?use=$id")
            require(resp.status.isSuccess())
            return resp.bodyAsText()
        }
        catch (e: IllegalArgumentException) {
            awaitCancellation()
        }
        finally {
            client.get(url(8) + "?close=$id").bodyAsText()
        }
    }

    return try {
        withTimeout(Duration.ofSeconds(10)) { // temporary while broken
            raceOf({
                req()
            }, {
                req()
            })
        }
    }
    catch (e: TimeoutCancellationException) {
        "wrong"
    }
}


val scenarios = listOf(::scenario1, ::scenario2, ::scenario3, ::scenario4, ::scenario5, ::scenario6, ::scenario7, ::scenario8)
//val scenarios = listOf(::scenario6)

suspend fun results(url: (Int) -> String) = scenarios.map {
    coroutineScope {
        async { it(url) }
    }
}.awaitAll()

suspend fun main() {
    fun url(scenario: Int) = "http://localhost:8080/$scenario"
    println(results(::url))
}