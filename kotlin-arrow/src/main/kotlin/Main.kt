import arrow.core.merge
import arrow.fx.coroutines.raceN
import io.ktor.client.*
import io.ktor.client.request.*
import io.ktor.client.statement.*
import io.ktor.http.*
import kotlinx.coroutines.async
import kotlinx.coroutines.awaitAll
import kotlinx.coroutines.awaitCancellation
import kotlinx.coroutines.coroutineScope
import kotlinx.coroutines.time.delay
import kotlinx.coroutines.time.withTimeout
import java.io.IOException
import java.time.Duration


val client = HttpClient()

// Note: Intentionally, only url handling code is shared across scenarios

suspend fun scenario1(url: (Int) -> String): String =
    raceN({
        client.get(url(1))
    }, {
        client.get(url(1))
    }).merge().bodyAsText()


suspend fun scenario2(url: (Int) -> String): String {
    suspend fun req(): String =
        try {
            client.get(url(2)).bodyAsText()
        }
        catch (e: IOException) {
            awaitCancellation()
        }

    return raceN({
        req()
    }, {
        req()
    }).merge()

}


// todo: not working
suspend fun scenario3(url: (Int) -> String): String = coroutineScope {
    TODO()

    /*
    repeat(10_000) {
        client.get(url(3))
    }
     */
}


// todo: not working
suspend fun scenario4(url: (Int) -> String): String =
    raceN({
        withTimeout(Duration.ofSeconds(1)) {
            client.get(url(4))
        }
    }, {
        client.get(url(4))
    }).merge().bodyAsText()


suspend fun scenario5(url: (Int) -> String): String {
    suspend fun req(): String =
        try {
            val resp = client.get(url(5))
            require(resp.status.isSuccess())
            resp.bodyAsText()
        }
        catch (e: IllegalArgumentException) {
            awaitCancellation()
        }

    return raceN({
        req()
    }, {
        req()
    }).merge()
}


suspend fun scenario6(url: (Int) -> String): String {
    suspend fun req(): String =
        try {
            val resp = client.get(url(6))
            require(resp.status.isSuccess())
            resp.bodyAsText()
        }
        catch (e: IllegalArgumentException) {
            awaitCancellation()
        }

    return raceN({
        req()
    }, {
        req()
    }, {
        req()
    }).fold({it}, {it}, {it})
}


suspend fun scenario7(url: (Int) -> String): String =
    raceN({
        client.get(url(7))
    }, {
        delay(Duration.ofSeconds(3))
        client.get(url(7))
    }).merge().bodyAsText()


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

    return withTimeout(Duration.ofSeconds(10)) {
        raceN({
            req()
        }, {
            req()
        }).merge()
    }
}


val scenarios = listOf(::scenario1, ::scenario2, ::scenario3, ::scenario4, ::scenario5, ::scenario6, ::scenario7, ::scenario8)
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