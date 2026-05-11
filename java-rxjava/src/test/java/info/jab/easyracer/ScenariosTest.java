package info.jab.easyracer;

import io.reactivex.rxjava4.core.Single;
import io.reactivex.rxjava4.observers.TestObserver;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.containers.wait.strategy.HttpWaitStrategy;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;
import org.testcontainers.utility.DockerImageName;

import java.net.URI;
import java.time.Duration;
import java.util.Locale;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;
import java.util.stream.Stream;

import static org.assertj.core.api.Assertions.assertThat;

@Testcontainers
public class ScenariosTest {

    private static final String EASY_RACER_IMAGE = "ghcr.io/jamesward/easyracer";

    /**
     * Max wait for {@code observer.awaitDone}; scenario 10 alone can exceed 60s (random 5–10s blocker sleep,
     * 1s poll interval, CPU load checks). Matches typical Surefire slack without matching the fork cap (900s).
     */
    private static final int SCENARIO_AWAIT_SECONDS = 180;
    @Container
    private static final GenericContainer<?> scenarioServer =
        new GenericContainer<>(DockerImageName.parse(EASY_RACER_IMAGE)).withExposedPorts(8080)
            .withStartupTimeout(Duration.ofSeconds(5))
            .waitingFor(new HttpWaitStrategy().withStartupTimeout(Duration.ofSeconds(5)))
            //.withCommand("--debug")//DEBUG ONLY
            .withLogConsumer(outputFrame -> System.out.print(outputFrame.getUtf8String()));

    @AfterAll
    static void closeScenarioServer() {
        scenarioServer.close();
    }

    @Timeout(value = 15, unit = TimeUnit.SECONDS)
    @ParameterizedTest(name = "scenario {0}")
    @MethodSource("scenarios")
    void scenarioReturnsRight(int scenarioNumber, Function<Scenarios, Single<Scenarios.ScenarioResult>> runScenario)
        throws Exception {

        if (scenarioNumber == 3) {
            Assumptions.assumeTrue(
                System.getProperty("os.name", "").toLowerCase(Locale.ROOT).contains("linux"),
                "scenario 3 not run on this OS (Linux only)");
        }

        var url = new URI("http://" + scenarioServer.getHost() + ":" + scenarioServer.getFirstMappedPort());
        try (var scenarioPath = new Scenarios(url)) {
            TestObserver<Scenarios.ScenarioResult> observer = runScenario.apply(scenarioPath).test();
            try {
                observer.awaitDone(SCENARIO_AWAIT_SECONDS, TimeUnit.SECONDS);
                observer.assertComplete();
                observer.assertNoErrors();
                assertThat(observer.values())
                    .as("scenario %s", scenarioNumber)
                    .containsExactly(Scenarios.ScenarioResult.RIGHT);
            } finally {
                observer.dispose();
            }
        }
    }

    static Stream<Arguments> scenarios() {
        return Stream.of(
                scenarioCase(1, s -> s.scenario1()),
                scenarioCase(2, s -> s.scenario2()),
                scenarioCase(3, s -> s.scenario3()),
                scenarioCase(4, s -> s.scenario4()),
                scenarioCase(5, s -> s.scenario5()),
                scenarioCase(6, s -> s.scenario6()),
                scenarioCase(7, s -> s.scenario7()),
                scenarioCase(8, s -> s.scenario8()),
                scenarioCase(9, s -> s.scenario9()),
                scenarioCase(10, s -> s.scenario10()),
                scenarioCase(11, s -> s.scenario11()));
    }

    private static Arguments scenarioCase(int number, Function<Scenarios, Single<Scenarios.ScenarioResult>> runner) {
        return Arguments.of(number, runner);
    }
}
