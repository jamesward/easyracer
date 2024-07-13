import com.sun.management.OperatingSystemMXBean

import scala.concurrent.duration.*
import gears.async.*
import gears.async.default.given
import gears.async.Future.*
import okhttp3.*

import java.lang.management.ManagementFactory
import java.security.MessageDigest
import java.util.concurrent.TimeoutException
import java.util.concurrent.atomic.AtomicBoolean
import scala.annotation.tailrec
import scala.util.Random


case class OkHttpCallCancellable(call: Call) extends Cancellable:
  def cancel(): Unit =
    call.cancel()
    unlink()

  def execute(): Response = call.execute()

extension (call: Call)
  def link()(using Async): OkHttpCallCancellable =
    OkHttpCallCancellable(call).link()


case class OkHttpResponseCancellable(response: Response) extends Cancellable:
  def cancel(): Unit =
    response.close()
    unlink()

  def code: Int = response.code

  def isSuccessful: Boolean = response.isSuccessful

  def body(): ResponseBody = response.body()

extension (response: Response)
  def link()(using Async): OkHttpResponseCancellable =
    OkHttpResponseCancellable(response).link()


object EasyRacerClient:
  private val httpClient = OkHttpClient()
  private def scenarioRequest(url: String)(using Async) =
    httpClient.newCall(Request.Builder().url(url).build()).link()

  def scenario1(scenarioUrl: Int => String)(using Async.Spawn): String =
    val url = scenarioUrl(1)
    def req = Future(scenarioRequest(url).execute().link().body().string())
    Seq(req, req).awaitFirstWithCancel
    // Or:
    // req.orWithCancel(req).await.link().body().string()

  def scenario2(scenarioUrl: Int => String)(using Async.Spawn): String =
    val url = scenarioUrl(2)
    def req = Future(scenarioRequest(url).execute().link().body().string())
    Seq(req, req).awaitFirstWithCancel

  def scenario3(scenarioUrl: Int => String)(using Async.Spawn): String =
    val url = scenarioUrl(3)
    val reqs = Seq.fill(10000):
      Future(scenarioRequest(url).execute().link().body().string())
    reqs.awaitFirst

  def scenario4(scenarioUrl: Int => String)(using Async.Spawn): String =
    val url = scenarioUrl(4)
    def req = Future(scenarioRequest(url).execute().link().body().string())
    // Does not work:
//    Seq(withTimeout(1.second)(req), req).awaitFirst
    def timeout =
      AsyncOperations.sleep(1.second)
      Future.rejected(TimeoutException())
    Seq(req.orWithCancel(timeout), req).awaitFirst

  def scenario5(scenarioUrl: Int => String)(using Async.Spawn): String =
    val url = scenarioUrl(5)
    def req = Future:
      val resp = scenarioRequest(url).execute().link()
      require(resp.isSuccessful)
      resp.body().string()
    Seq(req, req).awaitFirstWithCancel

  def scenario6(scenarioUrl: Int => String)(using Async.Spawn): String =
    val url = scenarioUrl(6)
    def req = Future:
      val resp = scenarioRequest(url).execute().link()
      require(resp.isSuccessful)
      resp.body().string()
    Seq(req, req, req).awaitFirstWithCancel

  def scenario7(scenarioUrl: Int => String)(using Async.Spawn): String =
    val url = scenarioUrl(7)
    def req = Future(scenarioRequest(url).execute().link().body().string())
    def delayedReq =
      AsyncOperations.sleep(3.seconds)
      req
    Seq(req, delayedReq).awaitFirstWithCancel

  def scenario8(scenarioUrl: Int => String)(using Async.Spawn): String =
    def req(url: String) =
      val resp = scenarioRequest(url).execute().link()
      require(resp.isSuccessful)
      resp.body().string()

    def open = req(s"${scenarioUrl(8)}?open")
    def use(id: String) = req(s"${scenarioUrl(8)}?use=$id")
    def close(id: String) = req(s"${scenarioUrl(8)}?close=$id")

    def reqRes = Future:
      val id = open
      try use(id)
      finally close(id)

    Seq(reqRes, reqRes).awaitFirstWithCancel

  def scenario9(scenarioUrl: Int => String)(using Async.Spawn): String =
    val url = scenarioUrl(9)
    def req = Future:
      val resp = scenarioRequest(url).execute().link()
      if !resp.isSuccessful then 0L -> ""
      else System.nanoTime -> resp.body().string()

    Seq.fill(10)(req).awaitAllOrCancel
      .sortBy(_._1).map(_._2).mkString

  def scenario10(scenarioUrl: Int => String)(using Async.Spawn): String =
    val id = Random.nextString(8)
    val messageDigest = MessageDigest.getInstance("SHA-512")

    def req(url: String) = Future(scenarioRequest(url).execute())

    def blocking = Future:
      class BlockingCancellable extends Cancellable:
        private val cancelled = AtomicBoolean(false)

        override def cancel(): Unit =
          cancelled.compareAndSet(false, true)
          unlink()

        def start(): Unit =
          var result = Random.nextBytes(512)
          while (!cancelled.get())
            result = messageDigest.digest(result)
      new BlockingCancellable().link().start()

    def blocker =
      val url = s"${scenarioUrl(10)}?$id"
      Seq(req(url), blocking)

    @tailrec
    def reporter: String =
      val osBean = ManagementFactory.getPlatformMXBean(classOf[OperatingSystemMXBean])
      val load = osBean.getProcessCpuLoad * osBean.getAvailableProcessors
      val resp = req(s"${scenarioUrl(10)}?$id=$load").await.link()
      if resp.code == 302 then
        AsyncOperations.sleep(1.second)
        reporter
      else
        require(resp.isSuccessful)
        resp.body().string()

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
