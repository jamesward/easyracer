import io.ktor.client.*
import io.ktor.client.request.*
import io.ktor.client.statement.*
import io.ktor.http.*
import kotlinx.coroutines.async
import kotlinx.coroutines.awaitAll
import kotlinx.coroutines.coroutineScope
import kotlinx.coroutines.time.delay
import kotlinx.coroutines.time.withTimeout
import splitties.coroutines.launchRacer
import splitties.coroutines.race
import splitties.coroutines.raceOf
import java.time.Duration

@splitties.experimental.ExperimentalSplittiesApi
suspend fun main() {

    val client = HttpClient()

    fun url(scenario: Int) = "http://localhost:8080/$scenario"

    suspend fun scenario1() =
        raceOf({
            client.get(url(1))
        }, {
            client.get(url(1))
        }).bodyAsText()


    suspend fun scenario2() = coroutineScope {
        race {
            repeat(10_000) {
                launchRacer {
                    client.get(url(2))
                }
            }
        }.bodyAsText()
    }


    // todo: not working
    suspend fun scenario3() =
        raceOf({
            withTimeout(Duration.ofSeconds(1)) {
                client.get(url(3)).bodyAsText()
            }
        }, {
            client.get(url(3)).bodyAsText()
        })


    // todo: not working
    suspend fun scenario4(): String {
        suspend fun req(): String {
            val resp = client.get(url(4))
            require(resp.status.isSuccess())
            return resp.bodyAsText()
        }

        return raceOf({
            req()
        }, {
            req()
        })
    }


    suspend fun scenario5() =
        raceOf({
            client.get(url(5))
        }, {
            delay(Duration.ofSeconds(3))
            client.get(url(5))
        }).bodyAsText()

    // todo: not working
    suspend fun scenario6(): String {
        suspend fun req(): String {
            val id = client.get(url(6) + "?open").bodyAsText()
            try {
                val resp = client.get(url(6) + "?use=$id")
                require(resp.status.isSuccess())
                return resp.bodyAsText()
            }
            finally {
                client.get(url(6) + "?close=$id").bodyAsText()
            }
        }

        return raceOf({
            req()
        }, {
            req()
        })
    }


    val scenarios = listOf(::scenario1, ::scenario2, ::scenario3, ::scenario4, ::scenario5, ::scenario6)
    //val scenarios = listOf(::scenario6)
    val results = scenarios.map {
        coroutineScope {
            async { it() }
        }
    }.awaitAll()

    println(results)

    require(results.all { it == "right" })

}