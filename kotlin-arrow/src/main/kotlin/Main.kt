import arrow.core.Either
import arrow.core.merge
import arrow.fx.coroutines.*
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
import java.util.*
import kotlin.random.Random

suspend fun <A> ignoreException(block: suspend () -> A): A =
  Either.catch { block() }.mapLeft { awaitCancellation() }.merge()

suspend fun ResourceScope.client(): HttpClient =
  install({
    HttpClient {
      install(HttpTimeout)
      followRedirects = false
    }
  }) { client, _ -> client.close() }

// Note: Intentionally, only url handling code is shared across scenarios
suspend fun HttpClient.scenario1(url: (Int) -> String): String =
  raceN({ get(url(1)) }, { get(url(1)) }).merge().bodyAsText()

suspend fun HttpClient.scenario2(url: (Int) -> String): String {
  suspend fun req(): String =
    ignoreException { get(url(2)).bodyAsText() }

  return raceN({
    req()
  }, {
    req()
  }).merge()
}

// note: plain Kotlin, not Arrow
suspend fun HttpClient.scenario3(url: (Int) -> String): String = coroutineScope {
  val reqs = List(10_000) {
    async {
      get(url(3))
    }
  }

  val done = select {
    reqs.map { req ->
      req.onAwait.invoke {
        it.bodyAsText()
      }
    }
  }

  reqs.forEach {
    it.cancelAndJoin()
  }

  done
}


suspend fun HttpClient.scenario4(url: (Int) -> String): String =
  raceN({
    try {
      get(url(4)) {
        timeout {
          requestTimeoutMillis = 1000
        }
      }
    } catch (e: Exception) {
      awaitCancellation()
    }
  }, {
    get(url(4))
  }).merge().bodyAsText()

suspend fun HttpClient.scenario5(url: (Int) -> String): String {
  suspend fun req(): String =
    ignoreException {
      val resp = get(url(5))
      require(resp.status.isSuccess())
      resp.bodyAsText()
    }

  return raceN({
    req()
  }, {
    req()
  }).merge()
}

suspend fun HttpClient.scenario6(url: (Int) -> String): String {
  suspend fun req(): String =
    ignoreException {
      val resp = get(url(6))
      require(resp.status.isSuccess())
      resp.bodyAsText()
    }

  return raceN({
    req()
  }, {
    req()
  }, {
    req()
  }).fold({ it }, { it }, { it })
}


suspend fun HttpClient.scenario7(url: (Int) -> String): String =
  raceN({
    get(url(7))
  }, {
    delay(Duration.ofSeconds(4))
    get(url(7))
  }).merge().bodyAsText()

suspend fun HttpClient.scenario8(url: (Int) -> String): String {
  suspend fun req(): String {
    val id = get(url(8) + "?open").bodyAsText()
    try {
      val resp = get(url(8) + "?use=$id")
      require(resp.status.isSuccess())
      return resp.bodyAsText()
    } finally {
      get(url(8) + "?close=$id").bodyAsText()
    }
  }

  return raceN({
    ignoreException { req() }
  }, {
    ignoreException { req() }
  }).merge()
}

suspend fun HttpClient.scenario9(url: (Int) -> String): String {
  suspend fun req(): Pair<Instant, String>? =
    get(url(9)).takeIf { it.status.isSuccess() }
      ?.let { resp -> Instant.now() to resp.bodyAsText() }

  return (1..10)
    .parMap { req() }
    .filterNotNull()
    .sortedBy { it.first }
    .joinToString("") { it.second }
}

suspend fun HttpClient.scenario10(url: (Int) -> String): String {
  val id = UUID.randomUUID().toString()

  val messageDigest = MessageDigest.getInstance("SHA-512")

  suspend fun blocking() = coroutineScope {
    var result = Random.nextBytes(512)
    while (isActive) {
      result = messageDigest.digest(result)
    }
  }

  suspend fun blocker() =
    raceN(
      { get(url(10) + "?$id") },
      { async(Dispatchers.IO) { blocking() }.await() }
    )

  suspend fun reporter(): String = coroutineScope {
    val osBean = ManagementFactory.getPlatformMXBean(OperatingSystemMXBean::class.java)
    val load = osBean.processCpuLoad * osBean.availableProcessors
    val resp = get(url(10) + "?$id=$load")
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

  return parZip(
    { blocker() },
    { reporter() }
  ) { _, result -> result }
}

fun HttpClient.scenarios() = listOf(
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

//fun HttpClient.scenarios() = listOf(
//  ::scenario10
//)

suspend fun results(url: (Int) -> String) = resourceScope {
  client().scenarios().parMap { it(url) }
}

suspend fun main() {
  fun url(scenario: Int) = "http://localhost:8080/$scenario"
  println(results(::url))
}
