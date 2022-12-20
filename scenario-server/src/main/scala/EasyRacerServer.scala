import zio.*
import zio.http.*
import zio.http.model.{Method, Status}
import zio.concurrent.ReentrantLock

import java.time.Instant

object EasyRacerServer extends ZIOAppDefault:

  type SessionPromise = Promise[Nothing, Unit | Instant | Promise[Nothing, String]]
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
        raceCoordinatorRef <- Promise.make[Nothing, Unit | Instant | Promise[Nothing, String]].flatMap(Ref.make(_))
      yield
        Session(locker, numRequestsRef, raceCoordinatorRef)


  /*
  Once two concurrent requests have come in, the first request returns the right response before the second one which takes a long time
  */
  def scenario1(session: Session): Request => ZIO[Any, Nothing, Response] = { _ =>
    val r = for
      numAndPromise <- session.add()
      (num, promise) = numAndPromise
      resp <- if num == 1 then
        promise.await.map(_ => Response.text("right"))
      else
        promise.succeed(()) *> ZIO.never
    yield
      resp

    r.onExit { _ =>
      session.remove()
    }
  }


  /*
  Once two concurrent requests have come in, the first request returns the right response while the second closes the connection
  */
  def scenario2(session: Session): Request => ZIO[Any, Nothing, Response] = { _ =>
    val r = for
      numAndPromise <- session.add()
      (num, promise) = numAndPromise
      resp <- if num == 1 then
        promise.await *> ZIO.sleep(1.second) as Response.text("right")
      else
        promise.succeed(()) *> ZIO.interrupt
    yield
      resp

    r.onExit { _ =>
      session.remove()
    }
  }

  /*
  10000 concurrent requests gets a right response
  */
  def scenario3(session: Session): Request => ZIO[Any, Nothing, Response] = { _ =>
    val r = for
      numAndPromise <- session.add()
      (num, promise) = numAndPromise
      resp <- if num < 10000 then
        promise.await *> ZIO.never
      else
        promise.succeed(()).as(Response.text("right"))
    yield
      resp

    r.onExit { _ =>
      session.remove()
    }
  }

  /*
  Once a request is cancelled, other pending requests return a right response
  */
  def scenario4(session: Session): Request => ZIO[Any, Nothing, Response] = { _ =>
    val r = for
      numAndPromise <- session.add()
      (_, promise) = numAndPromise
      _ <- promise.await
    yield
      Response.text("right")

    r.onExit { exit =>
      val waiter = if exit.isFailure then
        for
          numAndPromise <- session.get()
          (_, promise) = numAndPromise
          _ <- promise.succeed(())
        yield
          ()
      else
        ZIO.unit

      waiter *> session.remove()
    }
  }

  /*
  Once two requests have come in, the first one returns a 500, while a later one is right
  */
  def scenario5(session: Session): Request => ZIO[Any, Nothing, Response] = { _ =>
    val r = for
      numAndPromise <- session.add()
      (num, promise) = numAndPromise
      resp <- if num == 1 then
        promise.await.as(Response(status = Status.InternalServerError, body = Body.fromString("wrong")))
      else
        for
          _ <- promise.succeed(())
          _ <- ZIO.sleep(1.second) // insure that the wrong response comes before the right one
        yield
          Response.text("right")
    yield
      resp

    r.onExit { _ =>
      session.remove()
    }
  }

  /*
  The first request is right when a second "hedge" request starts after two seconds
  */
  def scenario6(session: Session): Request => ZIO[Any, Nothing, Response] = { _ =>
    val r = for
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
          _ <- ZIO.never
        yield
          throw NotImplementedError("Never gonna happen")
    yield
      resp

    r.onExit { _ =>
      session.remove()
    }
  }

  /*
  Two "open" requests are made which return unique ids.
  Two "use" requests are made; one returns an error, the other waits.
  A "close" request is made using the id from the failed "use" request.
  After that "close" request, the other "use" request's response is returned as the "right" response.
  */
  def scenario7(session: Session): Request => ZIO[Any, Nothing, Response] = { req =>
    enum Cmd:
      case Open
      case Use(id: String)
      case Close(id: String)

    val maybeCmd: Option[Cmd] = req.url.queryParams.get("open").map(_ => Cmd.Open)
      .orElse(req.url.queryParams.get("use").map(_.asString).map(Cmd.Use(_)))
      .orElse(req.url.queryParams.get("close").map(_.asString).map(Cmd.Close(_)))

    maybeCmd match
      case Some(Cmd.Open) =>
        Random.nextUUID.map(id => Response.text(id.toString))

      case Some(Cmd.Use(id)) =>
        val r = for
          numAndPromise <- session.add()
          (num, promise) = numAndPromise
          resp <- if num == 1 then
            promise.await.as(Response(status = Status.InternalServerError, body = Body.fromString("wrong")))
          else
            for
              nextPromise <- Promise.make[Nothing, String]
              _ <- promise.succeed(nextPromise)
              pId <- nextPromise.await
            yield
              if id != pId then Response.text("right") else Response.text("wrong")
        yield
          resp

        r.onExit { _ =>
          session.remove()
        }

      case Some(Cmd.Close(id)) =>
        for
          numAndPromise <- session.get()
          (num, promise) = numAndPromise
          resp <-
            if num == 1 then
              for
                sessionData <- promise.await
                resp <- sessionData match
                  case p: Promise[Nothing, String] =>
                    p.succeed(id).as(Response.ok)
                  case _ =>
                    throw NotImplementedError("not gonna happen")
              yield
                resp
            else
              ZIO.succeed(Response.ok)
        yield
          resp

      case None =>
        ZIO.succeed(Response.status(Status.NotAcceptable))
  }

  // scenario8
  // retry
  // game prevention: request 6 (randomly chosen number) will be the winner, but we wait for x seconds to
  //   verify that no other requests come in after the winner
  //

  def app(sessions: Map[String, Request => ZIO[Any, Nothing, Response]]) = Http.collectZIO[Request] {
    case Method.GET -> Path.root =>
      ZIO.succeed(Response.ok)
    case req @ Method.GET -> Path.root / scenario =>
      sessions.get(scenario) match {
        case Some(f) => f(req)
        case None => ZIO.succeed(Response.status(Status.NotFound))
      }
  }

  def run =
    for
      session1 <- Session.make()
      session2 <- Session.make()
      session3 <- Session.make()
      session4 <- Session.make()
      session5 <- Session.make()
      session6 <- Session.make()
      session7 <- Session.make()
      sessions = Map(
        "1" -> scenario1(session1),
        "2" -> scenario2(session2),
        "3" -> scenario3(session3),
        "4" -> scenario4(session4),
        "5" -> scenario5(session5),
        "6" -> scenario6(session6),
        "7" -> scenario7(session7),
      )
      server <- Server.serve(app(sessions)).provide(Server.default)
    yield
      server