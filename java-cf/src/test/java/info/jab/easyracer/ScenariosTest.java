package info.jab.easyracer;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.containers.wait.strategy.HttpWaitStrategy;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;
import org.testcontainers.utility.DockerImageName;

import java.net.URI;
import java.util.Locale;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;
import java.util.stream.Stream;

import static org.assertj.core.api.Assertions.assertThat;

@Testcontainers
public class ScenariosTest {

    private static final Logger logger = LoggerFactory.getLogger(ScenariosTest.class);

    //Docker image
    private static final String EASY_RACER_IMAGE = "ghcr.io/jamesward/easyracer";

    @Container
    @SuppressWarnings("resource")
    private static final GenericContainer<?> scenarioServer = new GenericContainer<>(DockerImageName.parse(EASY_RACER_IMAGE))
        .withExposedPorts(8080)
        .waitingFor(new HttpWaitStrategy())
        //.withCommand("--debug") //DEBUG ONLY
        .withLogConsumer(outputFrame -> logger.info(outputFrame.getUtf8String()));

    @AfterAll
    static void closeScenarioServer() {
        scenarioServer.close();
    }

    @Timeout(value = 15, unit = TimeUnit.SECONDS)
    @ParameterizedTest(name = "scenario {0}")
    @MethodSource("scenarios")
    void scenarioReturnsRight(int scenarioNumber, Function<Scenarios, Scenarios.ScenarioResult> runScenario) throws Exception {
        if (scenarioNumber == 3) {
            Assumptions.assumeTrue(
                    System.getProperty("os.name", "").toLowerCase(Locale.ROOT).contains("linux"),
                    "scenario 3 not run on this OS (Linux only)");
        }

        // given
        var url = new URI("http://" + scenarioServer.getHost() + ":" + scenarioServer.getFirstMappedPort());
        
        // when
        Scenarios.ScenarioResult result;
        try (var scenarioPath = new Scenarios(url)) {
            result = runScenario.apply(scenarioPath);
        } catch (RuntimeException e) {
            throw new AssertionError("scenario " + scenarioNumber + " failed with exception", e);
        }

        // then
        assertThat(result)
                .as("scenario %s", scenarioNumber)
                .isEqualTo(Scenarios.ScenarioResult.RIGHT);
    }

    static Stream<Arguments> scenarios() {
        return Stream.of(
                scenarioCase(1, Scenarios::scenario1),
                scenarioCase(2, Scenarios::scenario2),
                scenarioCase(3, Scenarios::scenario3),
                scenarioCase(4, Scenarios::scenario4),
                scenarioCase(5, Scenarios::scenario5),
                scenarioCase(6, Scenarios::scenario6),
                scenarioCase(7, Scenarios::scenario7),
                scenarioCase(8, Scenarios::scenario8),
                scenarioCase(9, Scenarios::scenario9),
                scenarioCase(10, Scenarios::scenario10),
                scenarioCase(11, Scenarios::scenario11));
    }

    private static Arguments scenarioCase(int number, Function<Scenarios, Scenarios.ScenarioResult> runner) {
        return Arguments.of(number, runner);
    }
}
