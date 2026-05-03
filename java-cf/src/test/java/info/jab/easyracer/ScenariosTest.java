package info.jab.easyracer;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.containers.wait.strategy.HttpWaitStrategy;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;
import org.testcontainers.utility.DockerImageName;

import java.net.URI;
import java.util.function.Function;
import java.util.stream.Stream;

import static org.assertj.core.api.Assertions.assertThat;

@Testcontainers
public class ScenariosTest {

    private static final String EASY_RACER_IMAGE = "ghcr.io/jamesward/easyracer";

    @Container
    private static final GenericContainer<?> scenarioServer = new GenericContainer<>(DockerImageName.parse(EASY_RACER_IMAGE))
        .withExposedPorts(8080)
        .waitingFor(new HttpWaitStrategy())
        //.withCommand("--debug")//DEBUG ONLY
        .withLogConsumer(outputFrame -> System.out.print(outputFrame.getUtf8String()));

    @AfterAll
    static void closeScenarioServer() {
        scenarioServer.close();
    }

    @ParameterizedTest(name = "scenario {0}")
    @MethodSource("scenarios")
    void scenarioReturnsRight(int scenarioNumber, Function<Scenarios, Scenarios.Value> runScenario) throws Exception {
        // given
        var url = new URI("http://" + scenarioServer.getHost() + ":" + scenarioServer.getFirstMappedPort());
        var scenarioPath = new Scenarios(url);

        // when
        var result = runScenario.apply(scenarioPath);

        // then
        assertThat(result)
                .as("scenario %s", scenarioNumber)
                .isEqualTo(Scenarios.Value.RIGHT);
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

    private static Arguments scenarioCase(int number, Function<Scenarios, Scenarios.Value> runner) {
        return Arguments.of(number, runner);
    }
}
