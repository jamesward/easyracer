import zio.*
import zio.direct.*
import zio.http.*
import zio.concurrent.{ConcurrentMap, ReentrantLock}
import zio.logging.{ConsoleLoggerConfig, LogFilter, LogFormat, consoleLogger}

import java.time.Instant
import scala.annotation.unused

object EasyRacerServer extends ZIOAppDefault:

  private val wrong = Response(Status.InternalServerError, body = Body.fromString("wrong"))

  private type SessionPromise[T] = Promise[Nothing, T]

  case class Session[T](private val locker: ReentrantLock, private val numRequestsRef: Ref[Int], private val raceCoordinatorRef: Ref[SessionPromise[T]]):
    def get(): ZIO[Any, Nothing, (Int, SessionPromise[T])] =
      defer:
        locker.lock.run
        val numRequests = numRequestsRef.get.run
        val raceCoordinator = raceCoordinatorRef.get.run
        locker.unlock.run
        (numRequests, raceCoordinator)

    def add(): ZIO[Any, Nothing, (Int, SessionPromise[T])] =
      defer:
        locker.lock.run
        val numRequests = numRequestsRef.updateAndGet(_ + 1).run
        ZIO.log(s"Num concurrent connections = $numRequests").run
        val raceCoordinator = raceCoordinatorRef.get.run
        locker.unlock.run
        (numRequests, raceCoordinator)

    def remove(): ZIO[Any, Nothing, Unit] =
      defer:
        locker.lock.run
        val numRequests = numRequestsRef.updateAndGet(_ - 1).run
        ZIO.log(s"Num concurrent connections = $numRequests").run
        ZIO.when(numRequests == 0)(Promise.make.flatMap(raceCoordinatorRef.set)).run
        locker.unlock.run

  object Session:
    def make[T](): ZIO[Any, Nothing, Session[T]] =
      defer:
        val locker = ReentrantLock.make().run
        val numRequestsRef = Ref.make[Int](0).run
        val sessionPromise = Promise.make[Nothing, T].run
        val raceCoordinatorRef = Ref.make(sessionPromise).run
        Session(locker, numRequestsRef, raceCoordinatorRef)


  /*
  Once two concurrent requests have come in, the first request returns the right response before the second one which takes a long time
  */
  def scenario1(session: Session[Unit])(@unused request: Request): ZIO[Any, Nothing, Response] =
    defer:
      val (num, promise) = session.add().run
      if num == 1 then
        promise.await.as(Response.text("right")).run
      else
        promise.succeed(()).run
        ZIO.never.run

    .onExit: _ =>
      session.remove()


  /*
  Once two concurrent requests have come in, the first request returns the right response while the second closes the connection
  */
  def scenario2(session: Session[Unit])(@unused request: Request): ZIO[Any, Nothing, Response] =
    defer:
      val (num, promise) = session.add().run
      if num == 1 then
        promise.await.run
        ZIO.sleep(1.second).run
        Response.text("right")
      else
        promise.succeed(()).run
        ZIO.interrupt.run

    .onExit: _ =>
        session.remove()


  /*
  10000 concurrent requests gets a right response
  */
  def scenario3(session: Session[Unit])(@unused request: Request): ZIO[Any, Nothing, Response] =
    defer:
      val (num, promise) = session.add().run
      if num < 10_000 then
        promise.await.run
        ZIO.never.run
      else
        promise.succeed(()).run
        Response.text("right")

    .onExit: _ =>
      session.remove()


  /*
  Once a request is cancelled, other pending requests return a right response
  */
  def scenario4(session: Session[Unit])(@unused request: Request): ZIO[Any, Nothing, Response] =
    defer:
      val (_, promise) = session.add().run
      promise.await.run
      Response.text("right")

    .onExit: exit =>
      defer:
        if exit.isFailure then
          val (_ , promise) = session.get().run
          promise.succeed(()).run

        session.remove().run


  /*
  Once two requests have come in, the first one returns a 500, while a later one is right
  */
  def scenario5(session: Session[Unit])(@unused request: Request): ZIO[Any, Nothing, Response] =
    defer:
      val (num, promise) = session.add().run
      if num == 1 then
        promise.await.run
        wrong
      else
        promise.succeed(()).run
        ZIO.sleep(1.second).run // insure that the wrong response comes before the right one
        Response.text("right")

    .onExit: _ =>
      session.remove()


  /*
  Once three requests have come in, the first one returns a 500, while a later one is right
  */
  def scenario6(session: Session[Unit])(@unused request: Request): ZIO[Any, Nothing, Response] =
    defer:
      val (num, promise) = session.add().run
      if num == 1 then
        promise.await.run
        wrong
      else if num == 2 then
        promise.await.run
        ZIO.sleep(1.second).run // insure that the wrong response comes before the right one
        Response.text("right")
      else
        promise.succeed(()).run
        ZIO.never.run

    .onExit: _ =>
      session.remove()


  /*
  The first request is right when a second "hedge" request starts after two seconds
  */
  def scenario7(session: Session[Instant])(@unused request: Request): ZIO[Any, Nothing, Response] =
    defer:
      val (num, promise) = session.add().run
      if num == 1 then
        val first = Clock.instant.run
        val second = promise.await.run
        if second.minusSeconds(2).isAfter(first) then
          Response.text("right")
        else
          Response.text("wrong")
      else
        val now = Clock.instant.run
        promise.succeed(now).run
        ZIO.never.run

    .onExit: _ =>
      session.remove()


  /*
  Two "open" requests are made which return unique ids.
  Two "use" requests are made; one returns an error, the other waits.
  A "close" request is made using the id from the failed "use" request.
  After that "close" request, the other "use" request's response is returned as the "right" response.
  */
  def scenario8(session: Session[Promise[Nothing, String]])(request: Request): ZIO[Any, Nothing, Response] =
    enum Cmd:
      case Open
      case Use(id: String)
      case Close(id: String)

    val maybeCmd: Option[Cmd] = request.url.queryParams.get("open").map(_ => Cmd.Open)
      .orElse(request.url.queryParams.get("use").map(Cmd.Use(_)))
      .orElse(request.url.queryParams.get("close").map(Cmd.Close(_)))

      maybeCmd match
        case Some(Cmd.Open) =>
          defer:
            val id = Random.nextUUID.run
            Response.text(id.toString)

        case Some(Cmd.Use(id)) =>
          defer:
            val (num, promise) = session.add().run
            if num == 1 then
              promise.await.run
              wrong
            else
              val nextPromise = Promise.make[Nothing, String].run
              promise.succeed(nextPromise).run
              val pId = nextPromise.await.run
              if id != pId then Response.text("right") else Response.text("wrong")

          .onExit: _ =>
            session.remove()

        case Some(Cmd.Close(id)) =>
          defer:
            val (num, promise) = session.get().run
            if num == 1 then
              val sessionData = promise.await.run
              sessionData.succeed(id).run
            Response.ok

        case None =>
          ZIO.succeed(Response.status(Status.NotAcceptable))

  /*
  Ten concurrent requests, 5 of them return errors, 5 return the letters "r", "i", "g", "h", and "t" in that order
  */
  def scenario9(session: Session[Queue[Option[(Char, Int)]]])(@unused request: Request): ZIO[Any, Nothing, Response] =
    def letterOrFail(chars: Queue[Option[(Char, Int)]]): ZIO[Any, Nothing, Response] =
      defer:
        chars.take.run match
          case None =>
            wrong
          case Some(char, seconds) =>
            ZIO.sleep(seconds.seconds).run
            Response.text(char.toString)

    defer:
      val (num, promise) = session.add().run
      if num < 10 then
        val sessionData = promise.await.run
        letterOrFail(sessionData).run
      else
        // create a queue with randomly dispersed chars
        val queue = Queue.bounded[Option[(Char, Int)]](10).run
        val right = "right".zipWithIndex
        val all = List.fill(5)(None) ++ right.map(Some(_))
        val allMixed = Random.shuffle(all).run
        queue.offerAll(allMixed).run
        promise.succeed(queue).run
        letterOrFail(queue).run

    .onExit: _ =>
      session.remove()


  case class Scenario10Data(readings: Seq[Double], duration: Duration, startTime: Instant):
    def isInBlocking(instant: Instant): Boolean =
      instant.isBefore(startTime.plusSeconds(duration.getSeconds))

  def scenario10(sessions: ConcurrentMap[String, Scenario10Data])(request: Request): ZIO[Any, Nothing, Response] =
    defer:
      request.url.queryParams.map.keys.headOption match
        case None =>
          Response(Status.BadRequest, body = Body.fromString("You need to specify a query parameter"))
        case Some(id) =>
          val now = Clock.instant.run

          request.url.queryParams.get(id).filterNot(_.isEmpty) match
            case None =>
              // the blocker
              val duration = Random.nextIntBetween(5, 10).run.seconds
              sessions.put(id, Scenario10Data(Seq.empty, duration, now)).run
              ZIO.log(s"Starting blocker for $duration").run
              ZIO.sleep(duration).run
              Response.ok
            case Some(loadString) =>
              loadString.toDoubleOption match
                case None =>
                  Response(Status.BadRequest, body = Body.fromString("load was not a double"))
                case Some(load) =>
                  sessions.get(id).run match
                    case None =>
                      // blocker hasn't started yet
                      Response.status(Status.Found)
                    case Some(data) =>
                      if data.isInBlocking(now) then
                        ZIO.log(s"Load while blocking: $load").run
                        val newData = data.copy(readings = data.readings :+ load)
                        sessions.put(id, newData).run
                        Response.status(Status.Found)
                      else
                        if data.readings.size < data.duration.getSeconds - 1 then
                          Response(Status.BadRequest, body = Body.fromString("Not enough readings"))
                        else
                          val meanLoad = data.readings.sum / data.readings.size
                          ZIO.log(s"Mean load while connected to blocker = $meanLoad, Current load = $load").run
                          if load > 0.3 then
                            Response(Status.Found, body = Body.fromString(s"Load was still too high: $load"))
                          else if meanLoad < 0.9 then
                            Response(Status.BadRequest, body = Body.fromString(s"A CPU was not near fully loaded - mean load = $meanLoad"))
                          else
                            Response.text("right")


  def app(scenarios: Seq[Request => ZIO[Any, Nothing, Response]]): Routes[Any, Nothing] =
    val index = Routes:
      Method.GET / "" ->
        Handler.ok

    val scenarioRoutes: Routes[Any, Nothing] =
      scenarios.zipWithIndex.foldLeft(Routes.empty): (routes, scenarioWithIndex) =>
        val (scenario, index) = scenarioWithIndex
        val scenarioNum = (index + 1).toString
        val route = Routes:
          Method.GET / scenarioNum ->
            Handler.fromFunctionZIO[Request]: request =>
              ZIO.logAnnotate("scenario", scenarioNum):
                scenario(request)
        routes ++ route

    index ++ scenarioRoutes


  private val isDebug =
    defer:
      val args = ZIOAppArgs.getArgs.run
      args.contains("--debug")


  override val bootstrap: ZLayer[ZIOAppArgs, Any, Any] =
    ZLayer.fromZIO:
      defer:
        if isDebug.run then
          val loggingConfig = ConsoleLoggerConfig(LogFormat.annotations |-| LogFormat.line, LogFilter.acceptAll)
          Runtime.removeDefaultLoggers >>> consoleLogger(loggingConfig)
        else
          Runtime.removeDefaultLoggers
    .flatten


  override def run =
    val scenariosBuilder =
      defer:
        Seq(
          scenario1(Session.make().run),
          scenario2(Session.make().run),
          scenario3(Session.make().run),
          scenario4(Session.make().run),
          scenario5(Session.make().run),
          scenario6(Session.make().run),
          scenario7(Session.make().run),
          scenario8(Session.make().run),
          scenario9(Session.make().run),
          scenario10(ConcurrentMap.empty.run),
        )

    val server =
      defer:
        val scenarios = scenariosBuilder.run
        val port = Server.install(app(scenarios).toHttpApp).run
        val debug = isDebug.run
        Console.printLine(s"Started server on port: $port (debug=$debug)").run

    (server *> ZIO.never).provideSomeLayer(Server.default)
