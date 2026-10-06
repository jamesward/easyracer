/*
 * Copyright (c) 2026, Antonio Gabriel Muñoz Conejo <me AT tonivade DOT es>
 * Distributed under the terms of the MIT License
 */
package es.tonivade.racer;

import static com.github.tonivade.diesel.Result.success;
import static java.util.function.Function.identity;
import static org.junit.jupiter.api.Assertions.assertEquals;
import org.junit.jupiter.api.Test;

import com.github.tonivade.diesel.Either;

class ScenariosTest {

  private final HttpImpl state = new HttpImpl();

  @Test
  void scenario1() {
    var scenario = Scenarios.scenario1();

    var result = scenario.eval(state).map(this::merge);

    assertEquals(success("right"), result);
  }

  @Test
  void scenario2() {
    var scenario = Scenarios.scenario2();

    var result = scenario.eval(state).map(this::merge);

    assertEquals(success("right"), result);
  }

  @Test
  void scenario3() {
    var scenario = Scenarios.scenario3();

    var result = scenario.eval(state);

    assertEquals(success("right"), result);
  }

  @Test
  void scenario4() {
    var scenario = Scenarios.scenario4();

    var result = scenario.eval(state).map(this::merge);

    assertEquals(success("right"), result);
  }

  @Test
  void scenario5() {
    var scenario = Scenarios.scenario5();

    var result = scenario.eval(state).map(this::merge);

    assertEquals(success("right"), result);
  }

  @Test
  void scenario6() {
    var scenario = Scenarios.scenario6();

    var result = scenario.eval(state);

    assertEquals(success("right"), result);
  }

  @Test
  void scenario7() {
    var scenario = Scenarios.scenario7();

    var result = scenario.eval(state);

    assertEquals(success("right"), result);
  }

  @Test
  void scenario8() {
    var scenario = Scenarios.scenario8();

    var result = scenario.eval(state);

    assertEquals(success("right"), result);
  }

  @Test
  void scenario9() {
    var scenario = Scenarios.scenario9();

    var result = scenario.eval(state);

    assertEquals(success("right"), result);
  }

  private String merge(Either<String, String> either) {
    return either.fold(identity(), identity());
  }
}
