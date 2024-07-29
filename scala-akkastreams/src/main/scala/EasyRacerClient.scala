import akka.NotUsed
import akka.actor.ActorSystem
import akka.http.scaladsl.model.Uri.Query
import akka.http.scaladsl.model.{StatusCode, StatusCodes}
import com.sun.management.OperatingSystemMXBean

import java.lang.management.ManagementFactory
import java.security.MessageDigest
import scala.util.Random
//import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{HttpRequest, Uri}
import akka.stream.Attributes
import akka.stream.scaladsl.{Flow, Source}
import akka.util.ByteString
import okhttp3.{Dispatcher, OkHttpClient}

import scala.concurrent.Await
import scala.concurrent.duration.*
import scala.util.{Failure, Success, Try}


object EasyRacerClient:
  def scenario1(implicit httpGetFlow: Flow[Uri, Try[(StatusCode, String)], ?]): Source[String, NotUsed] =
    val path = Uri("/1")
    val req = Source.single(path).via(httpGetFlow).collect:
      case Success((_, body)) => body

    req.merge(req).take(1)

  def scenario2(implicit httpGetFlow: Flow[Uri, Try[(StatusCode, String)], ?]): Source[String, NotUsed] =
    val path = Uri("/2")
    val req = Source.single(path).via(httpGetFlow).collect:
      case Success((_, body)) => body

    req.merge(req).take(1)

  def scenario3(implicit httpGetFlow: Flow[Uri, Try[(StatusCode, String)], ?]): Source[String, NotUsed] =
    val path = Uri("/3")
    val req = Source.single(path).via(httpGetFlow).collect:
      case Success((_, body)) => body

    Seq.fill(10_000)(req).reduce(_ merge _).take(1)

  def scenario4(implicit httpGetFlow: Flow[Uri, Try[(StatusCode, String)], ?]): Source[String, NotUsed] =
    val path = Uri("/4")
    val req = Source.single(path).via(httpGetFlow)
    val reqWithTimeout = req.idleTimeout(1.second).recover(Failure(_))

    req.merge(reqWithTimeout)
      .collect:
        case Success((_, body)) => body
      .take(1)

  def scenario5(implicit httpGetFlow: Flow[Uri, Try[(StatusCode, String)], ?]): Source[String, NotUsed] =
    val path = Uri("/5")
    val req = Source.single(path).via(httpGetFlow).collect:
      case Success((status, body)) if status.isSuccess => body

    req.merge(req).take(1)

  def scenario6(implicit httpGetFlow: Flow[Uri, Try[(StatusCode, String)], ?]): Source[String, NotUsed] =
    val path = Uri("/6")
    val req = Source.single(path).via(httpGetFlow).collect:
      case Success((status, body)) if status.isSuccess => body

    Seq(req, req, req).reduce(_ merge _).take(1)

  def scenario7(implicit httpGetFlow: Flow[Uri, Try[(StatusCode, String)], ?]): Source[String, NotUsed] =
    val path = Uri("/7")
    val req = Source.single(path).via(httpGetFlow).collect:
      case Success((status, body)) if status.isSuccess => body
    val reqWithDelay = Source.single(()).delay(3.seconds).flatMapConcat(_ => req)

    req.merge(reqWithDelay).take(1)

  def scenario8(implicit httpGetFlow: Flow[Uri, Try[(StatusCode, String)], ?]): Source[String, NotUsed] =
    val path = Uri("/8")
    val req = httpGetFlow.map:
      _.collect:
        case (status, body) if status.isSuccess => body

    val open = Source.single(path.withQuery(Query("open" -> Query.EmptyValue))).via(req)
    val use = Flow[String].map(id => path.withQuery(Query("use" -> id))).via(req)
    val close = Flow[String].map(id => path.withQuery(Query("close" -> id))).via(req)

    val reqRes = open.flatMapConcat:                        // Open
      case Success(id) =>
        Source.single(id).via(use).flatMapConcat: result => // Use
          Source.single(id).via(close).flatMapConcat: _ =>  // Close
            result match
              case Success(result) => Source.single(result)
              case _ => Source.empty
      case Failure(e) => Source.failed(e)

    reqRes.merge(reqRes).take(1)

  def scenario9(implicit httpGetFlow: Flow[Uri, Try[(StatusCode, String)], ?]): Source[String, NotUsed] =
    val path = Uri("/9")
    val req = Source.single(path).via(httpGetFlow)
      .collect:
        case Success((status, body)) if status.isSuccess => body

    Seq.fill(10)(req).reduce(_ merge _).fold("")(_ + _)

  def scenario10(implicit httpGetFlow: Flow[Uri, Try[(StatusCode, String)], ?]): Source[String, NotUsed] =
    val path = Uri("/10")
    val id = Random.nextString(8)
    val messageDigest = MessageDigest.getInstance("SHA-512")

    val blocking = Source.repeat(())
      .scan(Random.nextBytes(512)): (bytes, unit) =>
        messageDigest.digest(bytes)
      .map(Left[Array[Byte], String])

    val blocker: Source[Either[Array[Byte], String], NotUsed] =
      Source.single(path.withQuery(Query(id -> Query.EmptyValue)))
        .via(httpGetFlow)
        .collect:
          case Success(status, body) if status.isSuccess => body
        .map(Right[Array[Byte], String])

    def reporter: Source[String, NotUsed] =
      val osBean = ManagementFactory.getPlatformMXBean(classOf[OperatingSystemMXBean])
      val load = osBean.getProcessCpuLoad * osBean.getAvailableProcessors
      Source.single(path.withQuery(Query(id -> load.toString)))
        .via(httpGetFlow)
        .flatMapConcat:
          case Success((StatusCodes.Found, _)) =>
            Source.single(()).delay(1.second).flatMapConcat(_ => reporter)
          case Success((StatusCodes.OK, body)) =>
            Source.single(body)
          case Failure(e) => Source.failed(e)
          case _ => Source.failed(RuntimeException())

    blocking.merge(blocker)
      .collect:
        case Right(text) => text
      .take(1)
      .merge(reporter)
      .dropWhile(_ == "")
      .take(1)

  def scenarios(implicit httpGetFlow: Flow[Uri, Try[(StatusCode, String)], ?]): Source[String, NotUsed] =
    Source(Seq(scenario1, scenario2, scenario3, scenario4, scenario5, scenario6, scenario7, scenario8, scenario9, scenario10)).flatMapConcat(identity)

@main def run(): Unit =
  import EasyRacerClient.*
  implicit val system: ActorSystem = ActorSystem("easyracer")
  import system.dispatcher
  val okHttpDispatcher = Dispatcher()
  okHttpDispatcher.setMaxRequests(10_000)
  okHttpDispatcher.setMaxRequestsPerHost(10_000)
  val httpFlow =
//    Http().outgoingConnection("localhost", 8080)
    OkHttpClient().newBuilder()
      .dispatcher(okHttpDispatcher)
      .build()
      .outgoingConnection("localhost", 8080)
  implicit val httpGetFlow: Flow[Uri, Try[(StatusCode, String)], ?] = Flow[Uri]
    .map(uri => HttpRequest(uri = uri))
    .via(
      httpFlow
        .flatMapConcat: resp =>
          resp.entity.dataBytes
            .fold(ByteString.empty)(_ ++ _)
            .map:
              dataBytes => (resp.status, dataBytes.utf8String)
        .map(Success(_))
        .recover(Failure(_))
    )
  Await.ready(
    scenarios()
      .addAttributes(
        Attributes.logLevels(
          onElement = Attributes.LogLevels.Info,
          onFinish = Attributes.LogLevels.Info,
          onFailure = Attributes.LogLevels.Error))
      .runForeach(println),
    Duration.Inf
  )
