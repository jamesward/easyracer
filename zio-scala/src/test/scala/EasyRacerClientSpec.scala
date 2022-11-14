import zio.http.*
import zio.http.model.Method
import zio.test.*
import zio.test.Assertion.*

import java.io.IOException

object EasyRacerClientSpec extends ZIOSpecDefault
  def spec = ???
  /*
  def spec = suite("flaky")(
    test("true should succeed") {
      for
        _ <- TestRandom.feedBooleans(true)
        resp <- Flaky.app(Request.get(URL.root))
        body <- resp.body.asString
      yield
        assertTrue(body == "hello, flaky")
    },

    test("false should fail") {
      for
        _ <- TestRandom.feedBooleans(false)
        resp <- Flaky.app(Request.get(URL.root)).exit
      yield
        assert(resp)(fails(isSome[IOException](anything)))
    },
  )
  */
