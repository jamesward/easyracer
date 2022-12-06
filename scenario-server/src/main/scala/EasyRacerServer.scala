import zio.*
import zio.stm.*
import zio.http.*
import zio.http.model.{Method, Status}
import zio.http.middleware.HttpMiddleware
import zio.concurrent.{ConcurrentMap, CyclicBarrier, ReentrantLock}

import java.io.IOException
import java.time.Instant
import java.util.UUID

object EasyRacerServer extends ZIOAppDefault:

  type SessionPromise = Promise[Nothing, Unit | Instant]
  case class Session(private val locker: ReentrantLock, private val numRequestsRef: Ref[Int], private val raceCoordinatorRef: Ref[SessionPromise]):
    def get(): ZIO[Any, Nothing, (Int, SessionPromise)] =
      for
        _ <- locker.lock
        numRequests <- numRequestsRef.get
        raceCoordinator <- raceCoordinatorRef.get
        _ <- locker.unlock
      yield
        (numRequests, raceCoordinator)

    def add(): ZIO[Any, Nothing, (Int, SessionPromise)] =
      for
        _ <- locker.lock
        numRequests <- numRequestsRef.updateAndGet(_ + 1)
        raceCoordinator <- raceCoordinatorRef.get
        _ <- locker.unlock
      yield
        (numRequests, raceCoordinator)

    def remove(): ZIO[Any, Nothing, Unit] =
      for
        _ <- locker.lock
        numRequests <- numRequestsRef.updateAndGet(_ - 1)
        _ <- if numRequests == 0 then Promise.make.flatMap(raceCoordinatorRef.set(_)) else ZIO.unit
        _ <- locker.unlock
      yield
        ()

  object Session:
    def make(): ZIO[Any, Nothing, Session] =
      for
        locker <- ReentrantLock.make()
        numRequestsRef <- Ref.make[Int](0)
        raceCoordinatorRef <- Promise.make[Nothing, Unit | Instant].flatMap(Ref.make(_))
      yield
        Session(locker, numRequestsRef, raceCoordinatorRef)

  /*
  10000 concurrent requests gets a right response
  */
  def scenario1(session: Session): ZIO[Any, Nothing, Response] =
    val r = for
      numAndPromise <- session.add()
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
      session.remove()
    }

  /*
  Once two concurrent requests have come in, the first request returns the right response before the second one which takes a long time
  */
  def scenario2(session: Session): ZIO[Any, Nothing, Response] =
    val r = for
      numAndPromise <- session.add()
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
      session.remove()
    }

  /*
  Once a request is cancelled, other pending requests return a right response
  */
  def scenario3(session: Session): ZIO[Any, Nothing, Response] =
    val r = for
      numAndPromise <- session.add()
      (_, promise) = numAndPromise
      _ <- promise.await
    yield
      Response.text("right")

    r.onExit { e =>
      val waiter = if e.isFailure then
        for
          numAndPromise <- session.get()
          (_, promise) = numAndPromise
          _ <- promise.succeed(())
        yield
          ()
      else
        ZIO.unit

      waiter &> session.remove()
    }

  /*
  Once two requests have come in, the first one returns a 500, while a later one is right
  */
  def scenario4(session: Session): ZIO[Any, Nothing, Response] =
    val r = for
      numAndPromise <- session.add()
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
      session.remove()
    }

  /*
  The first request is right when a second "hedge" request starts after two seconds
  */
  def scenario5(session: Session): ZIO[Any, Nothing, Response] =
    val r = for
      _ <- ZIO.unit
      numAndPromise <- session.add()
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
      session.remove()
    }

  def app(sessions: Map[String, ZIO[Any, Nothing, Response]]) = Http.collectZIO[Request] {
    case Method.GET -> Path.root / scenario =>
      sessions.getOrElse(scenario, ZIO.succeed(Response.text("asdf")))
  }

  def run =
    for
      session1 <- Session.make()
      session2 <- Session.make()
      session3 <- Session.make()
      session4 <- Session.make()
      session5 <- Session.make()
      sessions = Map("1" -> scenario1(session1), "2" -> scenario2(session2), "3" -> scenario3(session3), "4" -> scenario4(session4), "5" -> scenario5(session5))
      server <- Server.serve(app(sessions)).provide(Server.default)
    yield
      server
