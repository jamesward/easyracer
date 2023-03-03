import zio.Runtime
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
        req2 <- EasyRacerServer.scenario10(session)(Request.get(URL.empty.setQueryParams(s"$num1=$fib1")))
        res2 <- req2.body.asString
        req3 <- EasyRacerServer.scenario10(session)(Request.get(URL.empty.setQueryParams(s"$num2=$fib2")))
        res3 <- req3.body.asString
      yield
        assertTrue(res1.toInt == num1) &&
          assertTrue(res2.toInt == num2) &&
          assertTrue(res3 == "right")
    }
  ).provideLayer(Runtime.removeDefaultLoggers)
