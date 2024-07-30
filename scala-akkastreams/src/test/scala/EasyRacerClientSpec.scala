import akka.actor.ActorSystem
import akka.stream.scaladsl.{Sink, Source}
import com.dimafeng.testcontainers.GenericContainer
import io.netty.channel.nio.NioEventLoopGroup
import org.asynchttpclient.Dsl.*
import org.scalatest.BeforeAndAfterAll
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.{Seconds, Span}
import org.testcontainers.containers.wait.strategy.Wait
import org.testcontainers.images.PullPolicy

import java.util.concurrent.Executors
import scala.compiletime.uninitialized
import scala.concurrent.ExecutionContext

class EasyRacerClientSpec extends AnyFlatSpec with Matchers with ScalaFutures with BeforeAndAfterAll:
  var container: GenericContainer = uninitialized
  var port: Int = 0

  override protected def beforeAll(): Unit =
    super.beforeAll()
    container = GenericContainer(
      "ghcr.io/jamesward/easyracer",
      Seq(8080),
      waitStrategy = Wait.forHttp("/"),
      imagePullPolicy = PullPolicy.alwaysPull()
    )
    container.start()
    port = container.container.getFirstMappedPort

  override protected def afterAll(): Unit =
    container.stop()
    super.afterAll()

  implicit val defaultPatience: PatienceConfig = PatienceConfig(timeout = Span(30, Seconds))
  private val es = Executors.newCachedThreadPool()
  implicit private val system: ActorSystem = ActorSystem("easyracer")
  implicit private val ec: ExecutionContext = system.dispatcher
  private lazy val httpFlow =
    asyncHttpClient(
      config()
        .setEventLoopGroup(NioEventLoopGroup(100, es))
        .setMaxConnections(12_000)
        .setMaxConnectionsPerHost(12_000)
    ).outgoingConnection("localhost", port)

  EasyRacerClient.scenarios.zipWithIndex.foreach: (flow, number) =>
    s"scenario ${number + 1}" should "work" in:
      Source.single(httpFlow)
        .via(flow)
        .runWith(Sink.seq)
        .futureValue should equal (Seq("right"))
