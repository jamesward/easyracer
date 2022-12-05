import zio.*
import zio.stm.*
import zio.http.*
import zio.http.model.{Method, Status}
import zio.http.middleware.HttpMiddleware
import zio.concurrent.{ConcurrentMap, CyclicBarrier, ReentrantLock}

import java.io.IOException
import java.time.Instant
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
  case class Sessions(locker: ReentrantLock, concurrentMap: ConcurrentMap[Session, (Ref[Int], Ref[Promise[Nothing, Unit | Instant]])]):
    def get(session: Session): ZIO[Any, Nothing, (Ref[Int], Ref[Promise[Nothing, Unit | Instant]])] =
      for
        _ <- locker.lock
        maybeSession <- concurrentMap.get(session)
        refs <- maybeSession.fold {
          for
            i <- Ref.make(0)
            p <- Promise.make[Nothing, Unit | Instant].flatMap(Ref.make(_))
            _ <- concurrentMap.put(session, (i, p))
          yield
            (i, p)
        } { ZIO.succeed(_, _) }
        _ <- locker.unlock
      yield
        refs

    def add(session: Session): ZIO[Any, Nothing, (Int, Promise[Nothing, Unit | Instant])] =
      for
        refs <- get(session)
        (refI, refP) = refs
        i <- refI.updateAndGet(_ + 1)
        p <- refP.get
      yield
        (i, p)

    def remove(session: Session): ZIO[Any, Nothing, Unit] =
      for
        _ <- locker.lock
        maybeSession <- concurrentMap.get(session)
        _ <- maybeSession.fold(ZIO.unit) { (refI, refP) =>
          for
            i <- refI.updateAndGet(_ - 1)
            _ <- if i == 0 then Promise.make[Nothing, Unit | Instant].flatMap(refP.set(_)) else ZIO.unit
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

  /*
  10000 concurrent requests gets a right response
  */
  def scenario1(sessions: Sessions)(session: Session): ZIO[Any, Nothing, Response] =
    val r = for
      numAndPromise <- sessions.add(session)
      (num, promise) = numAndPromise
      resp <- if num < 10000 then
        for
          _ <- promise.await
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
      sessions.remove(session)
    }

  /*
  Once two concurrent requests have come in, the first request returns the right response before the second one which takes a long time
  */
  def scenario2(sessions: Sessions)(session: Session): ZIO[Any, Nothing, Response] =
    val r = for
      numAndPromise <- sessions.add(session)
      (num, promise) = numAndPromise
      resp <- if num == 1 then
        promise.await.map(_ => Response.text("right"))
      else
        for
          _ <- promise.succeed(())
          _ <- ZIO.sleep(1.hour)
        yield
          Response.text("wrong")
    yield
      resp

    r.onExit { _ =>
      sessions.remove(session)
    }

  /*
  Once a request is cancelled, other pending requests return a right response
  */
  def scenario3(sessions: Sessions)(session: Session): ZIO[Any, Nothing, Response] =
    val r = for
      numAndPromise <- sessions.add(session)
      (_, promise) = numAndPromise
      _ <- promise.await
    yield
      Response.text("right")

    r.onExit { e =>
      val waiter = if e.isFailure then
        for
          refs <- sessions.get(session)
          (_, refP) = refs
          p <- refP.get
          _ <- p.succeed(())
        yield
          ()
      else
        ZIO.unit

      waiter &> sessions.remove(session)
    }

  /*
  Once two requests have come in, the first one returns a 500, while a later one is right
  */
  def scenario4(sessions: Sessions)(session: Session): ZIO[Any, Nothing, Response] =
    val r = for
      numAndPromise <- sessions.add(session)
      (num, promise) = numAndPromise
      resp <- if num == 1 then
        promise.await.map(_ => Response(status = Status.InternalServerError, body = Body.fromString("wrong")))
      else
        for
          _ <- promise.succeed(())
          _ <- ZIO.sleep(2.second)
        yield
          Response.text("right")
    yield
      resp

    r.onExit { _ =>
      sessions.remove(session)
    }

  /*
  The first request is right when a second "hedge" request starts after two seconds
  */
  def scenario5(sessions: Sessions)(session: Session): ZIO[Any, Nothing, Response] =
    val r = for
      numAndPromise <- sessions.add(session)
      (num, promise) = numAndPromise
      resp <- if num == 1 then
        for
          first <- Clock.instant
          second <- promise.await
        yield
          second match {
            case i: Instant if i.minusSeconds(2).isAfter(first) =>
              Response.text("right")
            case _ =>
              Response.text("wrong")
          }
      else
        for
          now <- Clock.instant
          _ <- promise.succeed(now)
          _ <- ZIO.sleep(2.seconds)
        yield
          Response.text("wrong")
    yield
      resp

    r.onExit { _ =>
      sessions.remove(session)
    }

  def app(sessions: Sessions) = Http.collectZIO[Request] {
    case req @ Method.GET -> Path.root / "1" => withSession(req)(scenario1(sessions))
    case req @ Method.GET -> Path.root / "2" => withSession(req)(scenario2(sessions))
    case req @ Method.GET -> Path.root / "3" => withSession(req)(scenario3(sessions))
    case req @ Method.GET -> Path.root / "4" => withSession(req)(scenario4(sessions))
    case req @ Method.GET -> Path.root / "5" => withSession(req)(scenario5(sessions))
  }

  def run =
    for
      locker <- ReentrantLock.make()
      backing <- ConcurrentMap.empty[Session, (Ref[Int], Ref[Promise[Nothing, Unit | Instant]])]
      sessions = Sessions(locker, backing)
      server <- Server.serve(app(sessions)).provide(Server.default)
    yield
      server

