import akka.{Done, NotUsed}
import akka.http.scaladsl.model.*
import akka.stream.scaladsl.{Flow, Source}
import org.asynchttpclient.{AsyncHttpClient, ListenableFuture, Response}

import scala.concurrent.{ExecutionContext, Future}
import scala.jdk.FutureConverters.*

extension (client: AsyncHttpClient)
  def outgoingConnection(host: String, port: Int)(using ExecutionContext): Flow[HttpRequest, HttpResponse, NotUsed] =
    Flow[HttpRequest]
      .flatMapConcat:
        case HttpRequest(HttpMethods.GET, uri, Nil, HttpEntity.Empty, HttpProtocols.`HTTP/1.1`) =>
          Source.unfoldResourceAsync(
            create = () =>
              Future.successful(
                (
                  client.prepareGet(uri.toEffectiveHttpRequestUri(Uri.Host(host), port).toString).execute(),
                  Iterator(()) // To ensure no more than one read
                )
              ),
            read = (ahcRespFut, iter) =>
              ahcRespFut.asScala.map: ahcResp =>
                iter.nextOption.map(_ => ahcResp),
            close = (ahcRespFut, iter) =>
              ahcRespFut.cancel(true)
              Future.successful(Done)
          )
        case _ =>
          Source.failed(UnsupportedOperationException()) // Only support what we need
      .map: ahcResp =>
        HttpResponse(
          status = ahcResp.getStatusCode,
          headers = Nil,
          entity = HttpEntity(ahcResp.getResponseBody),
          protocol = HttpProtocols.`HTTP/1.1`
        )

extension [T](future: ListenableFuture[T])
  def asScala: Future[T] = future.toCompletableFuture.asScala
