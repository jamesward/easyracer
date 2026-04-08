import org.apache.pekko.NotUsed
import org.apache.pekko.http.scaladsl.model.*
import org.apache.pekko.stream.scaladsl.{Flow, Keep, Source}
import org.asynchttpclient.{AsyncHttpClient, ListenableFuture}

import scala.concurrent.{ExecutionContext, Future}
import scala.jdk.FutureConverters.*

extension (client: AsyncHttpClient)
  def outgoingConnection(host: String, port: Int)(using ExecutionContext): Flow[HttpRequest, HttpResponse, NotUsed] =
    Flow[HttpRequest]
      .flatMapConcat:
        case HttpRequest(HttpMethods.GET, uri, Nil, HttpEntity.Empty, HttpProtocols.`HTTP/1.1`) =>
          val ahcRespFut = client.prepareGet(uri.toEffectiveHttpRequestUri(Uri.Host(host), port).toString).execute()
          Source.future(ahcRespFut.toCompletableFuture.asScala)
            .watchTermination(): (_, done) =>
              done.onComplete(_ => ahcRespFut.cancel(true))
              NotUsed
        case _ =>
          Source.failed(UnsupportedOperationException()) // Only support what we need
      .map: ahcResp =>
        HttpResponse(
          status = ahcResp.getStatusCode,
          headers = Nil,
          entity = HttpEntity(ahcResp.getResponseBody),
          protocol = HttpProtocols.`HTTP/1.1`
        )
