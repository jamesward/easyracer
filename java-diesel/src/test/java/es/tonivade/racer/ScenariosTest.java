/*
 * Copyright (c) 2026, Antonio Gabriel Muñoz Conejo <me AT tonivade DOT es>
 * Distributed under the terms of the MIT License
 */
package es.tonivade.racer;

import static com.github.tonivade.diesel.Result.success;
import static org.junit.jupiter.api.Assertions.assertEquals;

import com.github.tonivade.diesel.Program;

import java.net.URI;
import java.util.concurrent.TimeUnit;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.TestInstance.Lifecycle;
import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Timeout.ThreadMode;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.containers.wait.strategy.Wait;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;
import org.testcontainers.utility.DockerImageName;

@Testcontainers
@TestInstance(Lifecycle.PER_CLASS)
class ScenariosTest {

  private static final int PORT = 8080;

  @SuppressWarnings("resource")
  @Container
  static GenericContainer<?> container = new GenericContainer<>(DockerImageName.parse("ghcr.io/jamesward/easyracer"))
    .withExposedPorts(PORT)
    .waitingFor(Wait.forHttp("/"));

  final HttpImpl state = new HttpImpl();

  Scenarios scenarios;

  @BeforeAll
  void setUp() {
    var baseUrl = URI.create("http://" + container.getHost() + ":" + container.getMappedPort(PORT));
    scenarios = new Scenarios(baseUrl);
  }

  @Test
  @Timeout(unit = TimeUnit.SECONDS, value = 30, threadMode = ThreadMode.SEPARATE_THREAD)
  void scenario1() {
    verifyScenario(scenarios.scenario1());
  }

  @Test
  @Timeout(unit = TimeUnit.SECONDS, value = 30, threadMode = ThreadMode.SEPARATE_THREAD)
  void scenario2() {
    verifyScenario(scenarios.scenario2());
  }

  @Test
  @Timeout(unit = TimeUnit.SECONDS, value = 30, threadMode = ThreadMode.SEPARATE_THREAD)
  void scenario3() {
    verifyScenario(scenarios.scenario3());
  }

  @Test
  @Timeout(unit = TimeUnit.SECONDS, value = 30, threadMode = ThreadMode.SEPARATE_THREAD)
  void scenario4() {
    var scenario = scenarios.scenario4();

    verifyScenario(scenario);
  }

  @Test
  @Timeout(unit = TimeUnit.SECONDS, value = 30, threadMode = ThreadMode.SEPARATE_THREAD)
  void scenario5() {
    verifyScenario(scenarios.scenario5());
  }

  @Test
  @Timeout(unit = TimeUnit.SECONDS, value = 30, threadMode = ThreadMode.SEPARATE_THREAD)
  void scenario6() {
    verifyScenario(scenarios.scenario6());
  }

  @Test
  @Timeout(unit = TimeUnit.SECONDS, value = 30, threadMode = ThreadMode.SEPARATE_THREAD)
  void scenario7() {
    verifyScenario(scenarios.scenario7());
  }

  @Test
  @Timeout(unit = TimeUnit.SECONDS, value = 30, threadMode = ThreadMode.SEPARATE_THREAD)
  void scenario8() {
    var scenario = scenarios.scenario8();

    verifyScenario(scenario);
  }

  @Test
  @Timeout(unit = TimeUnit.SECONDS, value = 30, threadMode = ThreadMode.SEPARATE_THREAD)
  void scenario9() {
    verifyScenario(scenarios.scenario9());
  }

  @Test
  @Timeout(unit = TimeUnit.SECONDS, value = 30, threadMode = ThreadMode.SEPARATE_THREAD)
  void scenario10() {
    verifyScenario(scenarios.scenario10());
  }

  @Test
  @Timeout(unit = TimeUnit.SECONDS, value = 30, threadMode = ThreadMode.SEPARATE_THREAD)
  void scenario11() {
    verifyScenario(scenarios.scenario11());
  }

  private <E> void verifyScenario(Program<Http, E, String> scenario) {
    var result = scenario.eval(state);

    assertEquals(success("right"), result);
  }
}
