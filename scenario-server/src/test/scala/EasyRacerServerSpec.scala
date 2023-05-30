import zio.{Chunk, Runtime, ZIO}
import zio.http.{Request, URL}
import zio.test.*
import zio.test.Assertion.*

object EasyRacerServerSpec extends ZIOSpecDefault:

  def spec = suite("EasyRacerServer")(
    test("Scenario1") {
      for
        session <- EasyRacerServer.Session.make()
        req = EasyRacerServer.scenario1(session)(Request.get(URL.empty))
        winner <- req.race(req)
        body <- winner.body.asString
      yield
        assertTrue(body == "right")
    },
    test("Scenario10") {
      val num1 = 6
      val fib1 = 8
      val num2 = 7
      val fib2 = 13
      for
        _ <- TestRandom.feedInts(num1, num2)
        session <- EasyRacerServer.Session.make()
        req1 <- EasyRacerServer.scenario10(session)(Request.get(URL.empty))
        res1 <- req1.body.asString
        req2 <- EasyRacerServer.scenario10(session)(Request.get(URL.empty.withQueryParams(s"$num1=$fib1")))
        res2 <- req2.body.asString
        req3 <- EasyRacerServer.scenario10(session)(Request.get(URL.empty.withQueryParams(s"$num2=$fib2")))
        res3 <- req3.body.asString
      yield
        assertTrue(res1.toInt == num1, res2.toInt == num2, res3 == "right")
    },
    test("Scenario11") {
      val num1 = 6
      val fib1 = 8
      val num2 = 7
      val fib2 = 13
      for
        _ <- TestRandom.feedInts(num1, num2)
        session <- EasyRacerServer.Session.make()
        req1 <- EasyRacerServer.scenario11(session)(Request.get(URL.empty))
        res1 <- req1.body.asString
        Array(numFib1, numFib2) = res1.split(',')
        url = URL.empty.withQueryParams(numFib1 -> Chunk(fib1.toString), numFib2 -> Chunk(fib2.toString))
        req2 <- EasyRacerServer.scenario11(session)(Request.get(url))
        res2 <- req2.body.asString
      yield
        assertTrue(res2 == "right")
    },
  ).provideLayer(Runtime.removeDefaultLoggers)
