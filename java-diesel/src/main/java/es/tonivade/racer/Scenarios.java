/*
 * Copyright (c) 2026, Antonio Gabriel Muñoz Conejo <me AT tonivade DOT es>
 * Distributed under the terms of the MIT License
 */
package es.tonivade.racer;

import static com.github.tonivade.diesel.Combine.pipe;
import static com.github.tonivade.diesel.Concurrent.either;
import static com.github.tonivade.diesel.Concurrent.parAny;
import static com.github.tonivade.diesel.Concurrent.parSequence;
import static com.github.tonivade.diesel.Program.bracket;
import static com.github.tonivade.diesel.Program.catchAll;
import static com.github.tonivade.diesel.Program.delayed;
import static es.tonivade.racer.HttpDsl.get;
import static java.net.http.HttpResponse.BodyHandlers.ofString;
import static java.util.stream.Collectors.joining;
import static java.util.stream.IntStream.rangeClosed;

import java.net.URI;
import java.time.Duration;
import java.util.concurrent.Executor;
import java.util.concurrent.Executors;

import com.github.tonivade.diesel.Either;
import com.github.tonivade.diesel.Program;

public class Scenarios {

  private static final Executor executor = Executors.newVirtualThreadPerTaskExecutor();

  private static final URI BASE_URI = URI.create("http://localhost:8080/1");

  static Program<Http, String, Either<String, String>> scenario1() {
    return either(call("/1"), call("/1"), executor);
  }

  static Program<Http, String, Either<String, String>> scenario2() {
    return either(call("/2"), call("/2"), executor);
  }

  static Program<Http, String, String> scenario3() {
    var program = call("/3");
    var requests = rangeClosed(1, 10_000).mapToObj(_ -> program).toList();
    return parAny(executor, requests);
  }

  static Program<Http, String, Either<String, String>> scenario4() {
    return either(
        call("/4"),
        call("/4").timeout(Duration.ofSeconds(1)), executor);
  }

  static Program<Http, String, Either<String, String>> scenario5() {
    return either(call("/5"), call("/5"), executor);
  }

  static Program<Http, String, String> scenario6() {
    return parAny(executor,
        call("/6"),
        call("/6"),
        call("/6"));
  }

  static Program<Http, String, String> scenario7() {
    return parAny(executor,
        call("/7"),
        delayed(Duration.ofSeconds(3), call("/7")));
  }

  static Program<Http, String, String> scenario8() {
    var program = bracket(
        call("/8?open"),
        id -> call("8?use=" + id),
        id -> call("8?close=" + id));
    return parAny(executor, program, program);
  }

  static Program<Http, String, String> scenario9() {
    var program = call("/9");
    var requests = rangeClosed(1, 10).mapToObj(_ -> program).toList();
    return parSequence(executor, requests)
        .map(c -> c.stream().collect(joining()));
  }

  static Program<Http, String, String> scenario11() {
    var program = call("/11");
    return parAny(executor, program, parAny(executor, program, program));
  }

  private static Program<Http, String, String> call(String str) {
    return pipe(
        catchAll(get(request(str), ofString()), _ -> Program.failure("left")),
        r -> {
          if (r.statusCode() == 200) {
            return Program.success(r.body());
          }
          return Program.failure("left");
        });
  }

  private static URI request(String str) {
    return BASE_URI.resolve(str);
  }
}
