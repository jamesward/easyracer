import akka.NotUsed
import akka.actor.ActorSystem
import akka.http.scaladsl.model.Uri.Query
import akka.http.scaladsl.model.{HttpResponse, StatusCode, StatusCodes}
import com.sun.management.OperatingSystemMXBean
import io.netty.channel.nio.NioEventLoopGroup
import org.asynchttpclient.Dsl.*

import java.lang.management.ManagementFactory
import java.security.MessageDigest
import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import scala.util.Random
//import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{HttpRequest, Uri}
import akka.stream.scaladsl.{Flow, Source}
import akka.util.ByteString

import scala.concurrent.Await
import scala.concurrent.duration.*
import scala.util.{Failure, Success, Try}


object EasyRacerClient:
  type HttpFlow = Flow[HttpRequest, HttpResponse, NotUsed]
  
  private def scenarioRequestFlow(
    httpFlow: Flow[HttpRequest, HttpResponse, ?]
  ): Flow[Uri, Try[(StatusCode, String)], ?] = Flow[Uri]
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

  val scenario1: Flow[HttpFlow, String, NotUsed] =
    Flow[HttpFlow].map(scenarioRequestFlow).flatMapConcat: scenarioReq =>
      val path = Uri("/1")
      val req = Source.single(path).via(scenarioReq).collect:
        case Success((_, body)) => body

      req.merge(req).take(1)

  val scenario2: Flow[HttpFlow, String, NotUsed] =
    Flow[HttpFlow].map(scenarioRequestFlow).flatMapConcat: scenarioReq =>
      val path = Uri("/2")
      val req = Source.single(path).via(scenarioReq).collect:
        case Success((_, body)) => body

      req.merge(req).take(1)

  val scenario3: Flow[HttpFlow, String, NotUsed] =
    Flow[HttpFlow].map(scenarioRequestFlow).flatMapConcat: scenarioReq =>
      val path = Uri("/3")
      val req = Source.single(path).via(scenarioReq).collect:
        case Success((_, body)) => body
  
      Seq.fill(10_000)(req).reduce(_ merge _).take(1)

  val scenario4: Flow[HttpFlow, String, NotUsed] =
    Flow[HttpFlow].map(scenarioRequestFlow).flatMapConcat: scenarioReq =>
      val path = Uri("/4")
      val req = Source.single(path).via(scenarioReq)
      val reqWithTimeout = req.idleTimeout(1.second).recover(Failure(_))
  
      req.merge(reqWithTimeout)
        .collect:
          case Success((_, body)) => body
        .take(1)

  val scenario5: Flow[HttpFlow, String, NotUsed] =
    Flow[HttpFlow].map(scenarioRequestFlow).flatMapConcat: scenarioReq =>
      val path = Uri("/5")
      val req = Source.single(path).via(scenarioReq).collect:
        case Success((status, body)) if status.isSuccess => body
  
      req.merge(req).take(1)

  val scenario6: Flow[HttpFlow, String, NotUsed] =
    Flow[HttpFlow].map(scenarioRequestFlow).flatMapConcat: scenarioReq =>
      val path = Uri("/6")
      val req = Source.single(path).via(scenarioReq).collect:
        case Success((status, body)) if status.isSuccess => body
  
      Seq(req, req, req).reduce(_ merge _).take(1)

  val scenario7: Flow[HttpFlow, String, NotUsed] =
    Flow[HttpFlow].map(scenarioRequestFlow).flatMapConcat: scenarioReq =>
      val path = Uri("/7")
      val req = Source.single(path).via(scenarioReq).collect:
        case Success((status, body)) if status.isSuccess => body
      val reqWithDelay = Source.single(()).delay(3.seconds).flatMapConcat(_ => req)
  
      req.merge(reqWithDelay).take(1)

  val scenario8: Flow[HttpFlow, String, NotUsed] =
    Flow[HttpFlow].map(scenarioRequestFlow).flatMapConcat: scenarioReq =>
      val path = Uri("/8")
      val req = scenarioReq.map:
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

  val scenario9: Flow[HttpFlow, String, NotUsed] =
    Flow[HttpFlow].map(scenarioRequestFlow).flatMapConcat: scenarioReq =>
      val path = Uri("/9")
      val req = Source.single(path).via(scenarioReq)
        .collect:
          case Success((status, body)) if status.isSuccess => body
  
      Seq.fill(10)(req).reduce(_ merge _).fold("")(_ + _)

  val scenario10: Flow[HttpFlow, String, NotUsed] =
    Flow[HttpFlow].map(scenarioRequestFlow).flatMapConcat: scenarioReq =>
      val path = Uri("/10")
      val id = Random.nextString(8)
      val messageDigest = MessageDigest.getInstance("SHA-512")
  
      val blocking = Source.repeat(())
        .scan(Random.nextBytes(512)): (bytes, unit) =>
          messageDigest.digest(bytes)
        .map(Left[Array[Byte], String])
  
      val blocker: Source[Either[Array[Byte], String], NotUsed] =
        Source.single(path.withQuery(Query(id -> Query.EmptyValue)))
          .via(scenarioReq)
          .collect:
            case Success(status, body) if status.isSuccess => body
          .map(Right[Array[Byte], String])
  
      def reporter: Source[String, NotUsed] =
        val osBean = ManagementFactory.getPlatformMXBean(classOf[OperatingSystemMXBean])
        val load = osBean.getProcessCpuLoad * osBean.getAvailableProcessors
        Source.single(path.withQuery(Query(id -> load.toString)))
          .via(scenarioReq)
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

  val scenarios: Seq[Flow[HttpFlow, String, NotUsed]] =
    Seq(scenario1, scenario2, scenario3, scenario4, scenario5, scenario6, scenario7, scenario8, scenario9, scenario10)

@main def run(): Unit =
  import EasyRacerClient.*
  val es = Executors.newWorkStealingPool()
  implicit val system: ActorSystem = ActorSystem("easyracer", defaultExecutionContext = Some(ExecutionContext.fromExecutorService(es)))
  implicit val ec: ExecutionContext = system.dispatcher
  // Akka HTTP does not handle request cancellation, hence using AsyncHttpClient adapted to Akka Streams
  val httpFlow =
//    Http().outgoingConnection("localhost", 8080)
    asyncHttpClient(
      config()
        .setEventLoopGroup(NioEventLoopGroup(1, es))
        .setMaxConnections(10_000)
        .setMaxConnectionsPerHost(10_000)
    ).outgoingConnection("localhost", 8080)
  Await.ready(
    Source
      .single(httpFlow)
      .via(
        Flow[HttpFlow].flatMapConcat: httpFlow =>
          Source(scenarios).flatMapConcat(Source.single(httpFlow).via(_))
      )
      .runForeach(println),
    Duration.Inf
  )
