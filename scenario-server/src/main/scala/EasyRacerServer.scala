import zio.*
import zio.stm.*
import zio.http.*
import zio.http.model.{Method, Status}
import zio.http.middleware.HttpMiddleware
import zio.concurrent.{ConcurrentMap, CyclicBarrier, ReentrantLock}

import java.io.IOException
import java.util.UUID

opaque type Session = String
object Session:
  def apply(value: String): Session = value

object EasyRacerServer extends ZIOAppDefault:

  // this needs to be extracted to a more general structure and needs a session-lock instead of a global lock
  // design:
  //   concurrent requests need to share global state
  //   the global state keeps track of the number of concurrent requests
  //   a promise is managed such that any request can await or fulfill the promise
  //   the promise is reset when the number of concurrent requests is zero
  //   the caller gets the number of concurrent requests and the promise
  case class Sessions(locker: ReentrantLock, concurrentMap: ConcurrentMap[Session, (Ref[Int], Ref[Promise[Unit, Unit]])]):
    def get(session: Session): ZIO[Any, Nothing, (Int, Promise[Unit, Unit])] =
      for
        _ <- locker.lock
        maybeSession <- concurrentMap.get(session)
        refs <- maybeSession.fold {
          for
            i <- Ref.make(1)
            p <- Promise.make[Unit, Unit].flatMap(Ref.make(_))
            _ <- concurrentMap.put(session, (i, p))
          yield
            (i, p)
        } { (refI, refP) =>
          for
            _ <- refI.update(_ + 1)
          yield
            (refI, refP)
        }
        (refI, refP) = refs
        i <- refI.get
        p <- refP.get
        _ <- locker.unlock
      yield
        (i, p)

    def done(session: Session): ZIO[Any, Nothing, Unit] =
      for
        _ <- locker.lock
        maybeSession <- concurrentMap.get(session)
        _ <- maybeSession.fold(ZIO.unit) { (refI, refP) =>
          for
            i <- refI.updateAndGet(_ - 1)
            _ <- if i == 0 then Promise.make[Unit, Unit].flatMap(refP.set(_)) else ZIO.unit
          yield
            ()
        }
        _ <- locker.unlock
      yield
        ()



  def withSession(req: Request)(handler: Session => ZIO[Any, Nothing, Response]): ZIO[Any, Nothing, Response] =
    req.url.queryParams.get("user").fold {
      ZIO.succeed(Response.text("user query parameter required").setStatus(Status.UnprocessableEntity))
    } { chunks =>
      val user = chunks.asString
      val session = Session(user + req.url.path)
      handler(session)
    }

  def scenario1(sessions: Sessions)(session: Session): ZIO[Any, Nothing, Response] =
    val r = for
      numAndPromise <- sessions.get(session)
      (num, promise) = numAndPromise
      resp <- if num < 10000 then
        for
          _ <- promise.await.exit
          _ <- ZIO.sleep(10.seconds)
        yield
          Response.text("wrong")
      else
        for
          _ <- promise.succeed(())
        yield
          Response.text("right")
    yield
      resp

    r.onExit { _ =>
      sessions.done(session)
    }

  def scenario2(sessions: Sessions)(session: Session): ZIO[Any, Nothing, Response] =
    val r = for
      numAndPromise <- sessions.get(session)
      (num, promise) = numAndPromise
      resp <- if num == 1 then
        promise.await.exit.map(_ => Response.text("right"))
      else
        for
          _ <- promise.succeed(())
          _ <- ZIO.sleep(10.second)
        yield
          Response.text("wrong")
    yield
      resp

    r.onExit { _ =>
      sessions.done(session)
    }

  def app(sessions: Sessions) = Http.collectZIO[Request] {
    case req @ Method.GET -> Path.root / "1" => withSession(req)(scenario1(sessions))
    case req @ Method.GET -> Path.root / "2" => withSession(req)(scenario2(sessions))
  }

  def run =
    for
      locker <- ReentrantLock.make()
      backing <- ConcurrentMap.empty[Session, (Ref[Int], Ref[Promise[Unit, Unit]])]
      sessions = Sessions(locker, backing)
      server <- Server.serve(app(sessions)).provide(Server.default)
    yield
      server

