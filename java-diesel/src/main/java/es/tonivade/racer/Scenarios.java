/*
 * Copyright (c) 2026, Antonio Gabriel Muñoz Conejo <me AT tonivade DOT es>
 * Distributed under the terms of the MIT License
 */
package es.tonivade.racer;

import static com.github.tonivade.diesel.Concurrent.either;
import static com.github.tonivade.diesel.Concurrent.parAny;
import static com.github.tonivade.diesel.Concurrent.parSequence;
import static com.github.tonivade.diesel.Program.bracket;
import static com.github.tonivade.diesel.Program.delayed;
import static es.tonivade.racer.HttpDsl.get;
import static java.util.stream.Collectors.joining;

import java.net.URI;
import java.time.Duration;
import java.util.stream.IntStream;

import com.github.tonivade.diesel.Either;
import com.github.tonivade.diesel.Program;

public class Scenarios {

  private static final URI BASE_URI = URI.create("http://localhost:8080/1");

  static Program<Http, Throwable, Either<String, String>> scenario1() {
    return either(call("/1"), call("/1"));
  }

  static Program<Http, Throwable, Either<String, String>> scenario2() {
    return either(call("/2"), call("/2"));
  }

  static Program<Http, Throwable, String> scenario3() {
    var requests = IntStream.rangeClosed(1, 10_000)
        .mapToObj(_ -> call("/3")).toList();
    return parAny(requests);
  }

  static Program<Http, Throwable, Either<String, String>> scenario4() {
    return either(
        call("/4"),
        call("/4").timeout(Duration.ofSeconds(1)));
  }

  static Program<Http, Throwable, Either<String, String>> scenario5() {
    return either(call("/5"), call("/5"));
  }

  static Program<Http, Throwable, String> scenario6() {
    return parAny(
        call("/6"),
        call("/6"),
        call("/6"));
  }

  static Program<Http, Throwable, String> scenario7() {
    return parAny(
        call("/7"),
        delayed(Duration.ofSeconds(3), call("/7")));
  }

  static Program<Http, Throwable, String> scenario8() {
    var program = bracket(
        call("/8?open"),
        id -> call("8?use=" + id),
        id -> call("8?close=" + id));
    return parAny(program, program);
  }

  static Program<Http, Throwable, String> scenario9() {
    var requests = IntStream.rangeClosed(1, 10)
        .mapToObj(_ -> call("/9")).toList();
    return parSequence(requests)
        .map(c -> c.stream().collect(joining()));
  }

  private static Program<Http, Throwable, String> call(String str) {
    return get(request(str));
  }

  private static URI request(String str) {
    return BASE_URI.resolve(str);
  }
}
