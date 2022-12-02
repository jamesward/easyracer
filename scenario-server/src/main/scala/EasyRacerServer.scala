import zio.*
import zio.stm.*
import zio.http.*
import zio.http.model.{Method, Status}
import zio.http.middleware.HttpMiddleware
import zio.concurrent.{ConcurrentMap, CyclicBarrier, ReentrantLock}

import java.io.IOException
import java.util.UUID

opaque type User = String
object User:
  def apply(value: String): User = value

object EasyRacerServer extends ZIOAppDefault:

  // terrible
  // a user can have multiple concurrent calls to get but we need to single-thread it
  // this uses a global lock where it would be much better to have a per-user lock
  //
  // todo: key instead of user to support multiple scenarios
  case class Users(locker: ReentrantLock, concurrentMap: ConcurrentMap[User, CyclicBarrier]):
    def get(user: User): ZIO[Any, Nothing, CyclicBarrier] =
      for
        _ <- locker.lock
        maybeUser <- concurrentMap.get(user)
        barrier <- maybeUser.fold {
          for
            b <- CyclicBarrier.make(2) // configurable
            _ <- concurrentMap.put(user, b)
          yield
            b
        } { ZIO.succeed(_) }
        _ <- locker.unlock
      yield
        barrier


  def withUser(req: Request)(handler: User => ZIO[Any, Nothing, Response]): ZIO[Any, Nothing, Response] =
    req.url.queryParams.get("user").fold {
      ZIO.succeed(Response.text("user query parameter required").setStatus(Status.UnprocessableEntity))
    } { chunks =>
      val user = User(chunks.asString)
      handler(user)
    }

  def app(users: Users) = Http.collectZIO[Request] {
    case req @ Method.GET -> Path.root / "1" => withUser(req) { user =>
      for
        barrier <- users.get(user)
        waiting <- barrier.waiting
        _ = println(waiting)
        _ <- barrier.await.exit // exit to map the error from Unit to Nothing ?
        _ <- if waiting == 1 then ZIO.sleep(1.second) else ZIO.unit
      yield
        if waiting == 0 then Response.text("correct") else Response.text("wrong")
    }
  }

  def run =
    for
      locker <- ReentrantLock.make()
      backing <- ConcurrentMap.empty[User, CyclicBarrier]
      users = Users(locker, backing)
      server <- Server.serve(app(users)).provide(Server.default)
    yield
      server

