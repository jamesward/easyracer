import com.sun.management.OperatingSystemMXBean
import gears.async.*
import gears.async.Future.*
import gears.async.default.given
import io.netty.channel.nio.NioEventLoopGroup
import org.asynchttpclient
import org.asynchttpclient.Dsl.*

import java.lang.management.ManagementFactory
import java.security.MessageDigest
import java.util.concurrent.{ExecutionException, TimeoutException}
import java.util.concurrent.atomic.AtomicBoolean
import scala.annotation.tailrec
import scala.concurrent.duration.*
import scala.util.{Failure, Random, Try}


extension [T](ahcFuture: asynchttpclient.ListenableFuture[T])
  def asyncGet(using Async): T = Future
    .withResolver[T]: resolver =>
      ahcFuture.addListener(
        () =>
          try
            resolver.resolve(ahcFuture.get)
          catch
            case e: ExecutionException => resolver.reject(e.getCause)
            case other => resolver.reject(other),
        null
      )
      resolver.onCancel(() => ahcFuture.cancel(true))
    .link()
    .await


object EasyRacerClient:
  private val httpClient = asyncHttpClient(
    config()
      .setEventLoopGroup(NioEventLoopGroup(1))
      .setMaxConnections(10_000)
      .setMaxConnectionsPerHost(10_000)
  )

  private def scenarioRequest(url: String)(using Async) =
    httpClient.prepareGet(url).execute()

  def scenario1(scenarioUrl: Int => String)(using Async.Spawn): String =
    val url = scenarioUrl(1)
    def req = Future(scenarioRequest(url).asyncGet.getResponseBody)
    Seq(req, req).awaitFirstWithCancel
    // Or:
//    req.orWithCancel(req).await
    // Or:
//    Async.race(req, req).await

  def scenario2(scenarioUrl: Int => String)(using Async.Spawn): String =
    val url = scenarioUrl(2)
    def req = Future(scenarioRequest(url).asyncGet.getResponseBody)
    Seq(req, req).awaitFirstWithCancel
    // Or:
//    req.orWithCancel(req).await
    // Does not work (Async.race returns first to _complete_ - even if it completes with a failure):
//    Async.race(req, req).await

  def scenario3(scenarioUrl: Int => String)(using Async.Spawn): String =
    val url = scenarioUrl(3)
    val reqs = (0 until 10_000)
      .map: idx =>
        Future:
          // Uncomment on macOS
//          AsyncOperations.sleep((idx / 2).milliseconds)
          scenarioRequest(url).asyncGet.getResponseBody
    reqs.awaitFirstWithCancel

  def scenario4(scenarioUrl: Int => String)(using Async.Spawn): String =
    val url = scenarioUrl(4)
    def req(using Async) = scenarioRequest(url).asyncGet.getResponseBody
    Seq(Future(withTimeout(1.second)(req)), Future(req)).awaitFirstWithCancel


  def scenario5(scenarioUrl: Int => String)(using Async.Spawn): String =
    val url = scenarioUrl(5)
    def req = Future:
      val resp = scenarioRequest(url).get
      require(200 until 400 contains resp.getStatusCode)
      resp.getResponseBody
    Seq(req, req).awaitFirstWithCancel

  def scenario6(scenarioUrl: Int => String)(using Async.Spawn): String =
    val url = scenarioUrl(6)
    def req = Future:
      val resp = scenarioRequest(url).get
      require(200 until 400 contains resp.getStatusCode)
      resp.getResponseBody
    Seq(req, req, req).awaitFirstWithCancel

  def scenario7(scenarioUrl: Int => String)(using Async.Spawn): String =
    val url = scenarioUrl(7)
    def req = Future(scenarioRequest(url).asyncGet.getResponseBody)
    def delayedReq =
      AsyncOperations.sleep(3.seconds)
      req
    Seq(req, delayedReq).awaitFirstWithCancel

  def scenario8(scenarioUrl: Int => String)(using Async.Spawn): String =
    def req(url: String) =
      val resp = scenarioRequest(url).asyncGet
      require(200 until 400 contains resp.getStatusCode)
      resp.getResponseBody

    def open = req(s"${scenarioUrl(8)}?open")
    def use(id: String) = req(s"${scenarioUrl(8)}?use=$id")
    def close(id: String) = req(s"${scenarioUrl(8)}?close=$id")

    def reqRes = Future:
      val id = open
      try use(id)
      finally uninterruptible(close(id))

    Seq(reqRes, reqRes).awaitFirstWithCancel

  def scenario9(scenarioUrl: Int => String)(using Async.Spawn): String =
    val url = scenarioUrl(9)
    def req = Future:
      val resp = scenarioRequest(url).asyncGet
      if !(200 until 400 contains resp.getStatusCode) then 0L -> ""
      else System.nanoTime -> resp.getResponseBody

    Seq.fill(10)(req).awaitAllOrCancel
      .sortBy(_._1).map(_._2).mkString

  def scenario10(scenarioUrl: Int => String)(using Async.Spawn): String =
    val id = Random.nextString(8)
    val messageDigest = MessageDigest.getInstance("SHA-512")

    def req(url: String) = Future(scenarioRequest(url).asyncGet)

    def blocking = Future:
      class BlockingCancellable extends Cancellable:
        private val cancelled = AtomicBoolean(false)

        override def cancel(): Unit =
          cancelled.compareAndSet(false, true)
          unlink()

        def start(): Unit =
           @tailrec def digest(bytes: Array[Byte]): Unit =
             if !cancelled.get then digest(messageDigest.digest(bytes))
           digest(Random.nextBytes(512))
      new BlockingCancellable().link().start()
    // Or:
    // def blocking = Future:
    //   @tailrec def digest(bytes: Array[Byte]): Unit =
    //     // 0-time sleep introduces a suspension point for cancellation
    //     AsyncOperations.sleep(0.nanosecond)
    //     digest(messageDigest.digest(bytes))
    //   digest(Random.nextBytes(512))

    def blocker =
      val url = s"${scenarioUrl(10)}?$id"
      Seq(req(url), blocking)

    @tailrec
    def reporter: String =
      val osBean = ManagementFactory.getPlatformMXBean(classOf[OperatingSystemMXBean])
      val load = osBean.getProcessCpuLoad * osBean.getAvailableProcessors
      val resp = req(s"${scenarioUrl(10)}?$id=$load").await
      if resp.getStatusCode == 302 then
        AsyncOperations.sleep(1.second)
        reporter
      else
        require(200 until 400 contains resp.getStatusCode)
        resp.getResponseBody

    val result = Future(reporter)
    blocker.awaitFirstWithCancel
    result.await

  def scenarios: Seq[(Int => String) => Async.Spawn ?=> String] = Seq(
    scenario1, scenario2, scenario3, scenario4, scenario5, scenario6, scenario7, scenario8, scenario9, scenario10
  )

@main def run(): Unit =
  import EasyRacerClient.*
  def scenarioUrl(scenario: Int) = s"http://localhost:8080/$scenario"
  scenarios.foreach: s =>
    Async.blocking:
      println(s(scenarioUrl))
