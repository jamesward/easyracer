import akka.http.scaladsl.model.*
import akka.stream.scaladsl.{Flow, Source}
import akka.{Done, NotUsed}
import okhttp3.*

import java.io.IOException
import scala.concurrent.{ExecutionContext, Future, Promise}

extension (client: OkHttpClient)
  def outgoingConnection(host: String, port: Int)(using ExecutionContext): Flow[HttpRequest, HttpResponse, NotUsed] =
    Flow[HttpRequest]
      .flatMapConcat:
        case HttpRequest(HttpMethods.GET, uri, Nil, HttpEntity.Empty, HttpProtocols.`HTTP/1.1`) =>
          Source.unfoldResourceAsync(
            create = () =>
              Future.successful(
                client.newCall(
                  Request.Builder()
                    .url(uri.toEffectiveHttpRequestUri(Uri.Host(host), port).toString)
                    .build()
                )
              ),
            read = okHttpCall =>
              if okHttpCall.isExecuted then Future.successful(None)
              else okHttpCall.executeAsync().map(Some(_)),
            close = okHttpCall =>
              okHttpCall.cancel()
              Future.successful(Done)
          )
        case _ =>
          Source.failed(UnsupportedOperationException()) // Only support what we need
      .map: (okHttpResp: Response) =>
        HttpResponse(
          status = okHttpResp.code(),
          headers = Nil,
          entity = okHttpResp.asAkkaHttpResponseEntity(),
          protocol = HttpProtocols.`HTTP/1.1`
        )

extension (call: Call)
  def executeAsync()(using ExecutionContext): Future[Response] =
    val promise = Promise[Response]()
    call.enqueue(
      new Callback:
        def onFailure(c: Call, e: IOException): Unit = promise.failure(e)

        def onResponse(c: Call, r: Response): Unit = promise.success(r)
    )

    promise.future

extension (response: Response)
  def asAkkaHttpResponseEntity(): ResponseEntity = HttpEntity(response.body().string())