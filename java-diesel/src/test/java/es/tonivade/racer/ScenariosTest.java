/*
 * Copyright (c) 2026, Antonio Gabriel Muñoz Conejo <me AT tonivade DOT es>
 * Distributed under the terms of the MIT License
 */
package es.tonivade.racer;

import static com.github.tonivade.diesel.Result.success;
import static java.util.function.Function.identity;
import static org.junit.jupiter.api.Assertions.assertEquals;

import com.github.tonivade.diesel.Either;

import org.junit.jupiter.api.Test;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.containers.wait.strategy.Wait;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;
import org.testcontainers.utility.DockerImageName;

@Testcontainers
class ScenariosTest {

  private static final int PORT = 8080;

  @Container
  static GenericContainer<?> container = new GenericContainer<>(DockerImageName.parse("ghcr.io/jamesward/easyracer"))
    .withExposedPorts(PORT)
    .waitingFor(Wait.forHttp("/"));

  private final HttpImpl state = new HttpImpl();
  private final Scenarios scenarios = new Scenarios(container.getMappedPort(PORT));

  @Test
  void scenario1() {
    var scenario = scenarios.scenario1();

    var result = scenario.eval(state).map(this::merge);

    assertEquals(success("right"), result);
  }

  @Test
  void scenario2() {
    var scenario = scenarios.scenario2();

    var result = scenario.eval(state).map(this::merge);

    assertEquals(success("right"), result);
  }

  @Test
  void scenario3() {
    var scenario = scenarios.scenario3();

    var result = scenario.eval(state);

    assertEquals(success("right"), result);
  }

  @Test
  void scenario4() {
    var scenario = scenarios.scenario4();

    var result = scenario.eval(state).map(this::merge);

    assertEquals(success("right"), result);
  }

  @Test
  void scenario5() {
    var scenario = scenarios.scenario5();

    var result = scenario.eval(state).map(this::merge);

    assertEquals(success("right"), result);
  }

  @Test
  void scenario6() {
    var scenario = scenarios.scenario6();

    var result = scenario.eval(state);

    assertEquals(success("right"), result);
  }

  @Test
  void scenario7() {
    var scenario = scenarios.scenario7();

    var result = scenario.eval(state);

    assertEquals(success("right"), result);
  }

  @Test
  void scenario8() {
    var scenario = scenarios.scenario8();

    var result = scenario.eval(state);

    assertEquals(success("right"), result);
  }

  @Test
  void scenario9() {
    var scenario = scenarios.scenario9();

    var result = scenario.eval(state);

    assertEquals(success("right"), result);
  }

  @Test
  void scenario10() {
    var scenario = scenarios.scenario10();

    var result = scenario.eval(state);

    assertEquals(success("right"), result);
  }

  @Test
  void scenario11() {
    var scenario = scenarios.scenario11();

    var result = scenario.eval(state);

    assertEquals(success("right"), result);
  }

  private String merge(Either<String, String> either) {
    return either.fold(identity(), identity());
  }
}
