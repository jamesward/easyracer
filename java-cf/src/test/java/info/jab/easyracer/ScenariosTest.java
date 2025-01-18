package info.jab.easyracer;

import org.junit.jupiter.api.Test;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.containers.wait.strategy.HttpWaitStrategy;
import org.testcontainers.utility.DockerImageName;

import java.net.URI;

import static org.assertj.core.api.Assertions.assertThat;

public class ScenariosTest {

    private static final String EASY_RACER_IMAGE = "ghcr.io/jamesward/easyracer";

    @Test
    public void testScenarios() throws Exception {
        try (GenericContainer<?> scenarioServer = new GenericContainer<>(DockerImageName.parse(EASY_RACER_IMAGE))) {
            //Given
            scenarioServer.withExposedPorts(8080).waitingFor(new HttpWaitStrategy()).start();
            var url = new URI("http://" + scenarioServer.getHost() + ":" + scenarioServer.getFirstMappedPort());
            
            //When
            var scenarios = new Scenarios(url);
            var results = scenarios.results();

            //Then
            assertThat(results).allMatch(result -> result.equals("right"));
            assertThat(results).hasSize(8);
        }
    }

}