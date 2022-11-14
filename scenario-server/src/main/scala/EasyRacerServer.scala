import zio.http.*
import zio.http.model.Method
import zio.Console.printLine
import zio.{Random, ZIO, ZIOAppDefault, ZLayer}

import java.io.IOException

object EasyRacerServer extends ZIOAppDefault:

  val app = Http.collectZIO[Request] {
    case Method.GET -> Path.root =>
      for
        succeed <- Random.nextBoolean
        _ <- if succeed then printLine("not flaky") else printLine("flaky")
        result <-
          if succeed then
            ZIO.succeed(Response.text("hello, flaky"))
          else
            ZIO.fail(IOException("erggg"))
      yield result
  }

  def run =
    val serverConfig = ZLayer.succeed(ServerConfig.default.port(8081))
    Server.serve(app).provide(serverConfig >>> Server.default)

