import zio.*
import zio.http.*
import zio.http.model.Method
import zio.http.service.ChannelFactory

import java.io.IOException

object EasyRacerClient extends ZIOAppDefault:

  val url = "https://random-num-x5ht4amjia-uc.a.run.app/"

  val app =
    val r1 = Client.request(url)
    val r2 = Client.request(url)
    r1.race(r2).timeout(1.second).debug

  override val run =
    app.provide(Client.default, Scope.default)
