import zio.*
import zio.http.*
import zio.http.model.Method
import zio.http.service.ChannelFactory

import java.io.IOException

object EasyRacerClient extends ZIOAppDefault:

  val url = "http://localhost:8080/1?user=2"

  val app =
    val req = for
      resp <- Client.request(url)
      body <- resp.body.asString
    yield
      body

    req.race(req).debug

  override val run =
    app.provide(Client.default, Scope.default)
