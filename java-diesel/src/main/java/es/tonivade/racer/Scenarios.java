/*
 * Copyright (c) 2026, Antonio Gabriel Muñoz Conejo <me AT tonivade DOT es>
 * Distributed under the terms of the MIT License
 */
package es.tonivade.racer;

import static com.github.tonivade.diesel.Combine.pipe;
import static com.github.tonivade.diesel.Concurrent.parAny;
import static com.github.tonivade.diesel.Concurrent.parSequence;
import static com.github.tonivade.diesel.Concurrent.parZip;
import static com.github.tonivade.diesel.Program.bracket;
import static com.github.tonivade.diesel.Program.catchAll;
import static com.github.tonivade.diesel.Program.delayed;
import static com.github.tonivade.diesel.Program.failure;
import static com.github.tonivade.diesel.Program.interruptibleTask;
import static com.github.tonivade.diesel.Program.recursive;
import static com.github.tonivade.diesel.Program.success;
import static com.github.tonivade.diesel.function.Finisher2.last;
import static es.tonivade.racer.HttpDsl.get;
import static java.net.http.HttpResponse.BodyHandlers.ofString;
import static java.util.Comparator.comparing;
import static java.util.stream.Collectors.joining;
import static java.util.stream.IntStream.rangeClosed;

import com.github.tonivade.diesel.Program;
import com.sun.management.OperatingSystemMXBean;

import java.lang.management.ManagementFactory;
import java.net.URI;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.time.Duration;
import java.time.Instant;
import java.util.UUID;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadLocalRandom;
import java.util.function.Function;

public class Scenarios implements AutoCloseable {

  private final ExecutorService executor = Executors.newVirtualThreadPerTaskExecutor();

  private final URI baseUrl;

  public Scenarios(URI baseUrl) {
    this.baseUrl = baseUrl;
  }

  Program<Http, String, String> scenario1() {
    return parAny(executor, call("/1"), call("/1"));
  }

  Program<Http, String, String> scenario2() {
    return parAny(executor, call("/2"), call("/2"));
  }

  Program<Http, String, String> scenario3() {
    var program = call("/3");
    var requests = rangeClosed(1, 10_000).mapToObj(_ -> program).toList();
    return parAny(executor, requests);
  }

  Program<Http, String, String> scenario4() {
    return parAny(executor,
        call("/4"),
        call("/4").timeout(Duration.ofSeconds(1)));
  }

  Program<Http, String, String> scenario5() {
    return parAny(executor, call("/5"), call("/5"));
  }

  Program<Http, String, String> scenario6() {
    return parAny(executor,
        call("/6"),
        call("/6"),
        call("/6"));
  }

  Program<Http, String, String> scenario7() {
    return parAny(executor,
        call("/7"),
        delayed(Duration.ofSeconds(3), call("/7")));
  }

  Program<Http, String, String> scenario8() {
    var program = bracket(
        call("/8?open"),
        id -> call("/8?use=" + id),
        id -> call("/8?close=" + id));
    return parAny(executor, program, program);
  }

  Program<Http, Void, String> scenario9() {
    var program = callNoError("/9")
        .map(result -> new TimedResponse(Instant.now(), result));
    var requests = rangeClosed(1, 10).mapToObj(_ -> program).toList();
    return parSequence(executor, requests)
        .map(c -> c.stream()
            .sorted(comparing(TimedResponse::instant))
            .map(TimedResponse::value)
            .collect(joining()));
  }

  Program<Http, String, String> scenario10() {
    var id = UUID.randomUUID().toString();

    var part1 = parAny(executor, call("/10?" + id), heavyTask().map(_ -> ""));
    var part2 = recursiveTask().apply(id);

    return parZip(part1, part2, last(), executor);
  }

  Program<Http, String, String> scenario11() {
    return parAny(executor,
        call("/11"),
        parAny(executor, call("/11"), call("/11")));
  }

  @Override
  public void close() {
    executor.close();
  }

  private Function<String, Program<Http, String, String>> recursiveTask() {
    var osBean = ManagementFactory.getPlatformMXBean(OperatingSystemMXBean.class);
    return recursive((self, id) -> {
      var load = osBean.getProcessCpuLoad() * osBean.getAvailableProcessors();
      return catchAll(
        pipe(
            get(baseUrl.resolve("/10?" + id + "=" + load), ofString()),
            r -> {
              if (r.statusCode() >= 200 && r.statusCode() < 300) {
                return success(r.body());
              }
              if (r.statusCode() >= 300 && r.statusCode() < 400) {
                return delayed(Duration.ofSeconds(1), self.apply(id));
              }
              return failure("left");
            }),
        _ -> failure("left"));
    });
  }

  private Program<Http, String, Void> heavyTask() {
    return interruptibleTask(() -> {
      var digest = getDigest();
      byte[] array = new byte[512];
      ThreadLocalRandom.current().nextBytes(array);
      while (!Thread.interrupted()) {
        array = digest.digest(array);
      }
    });
  }

  private MessageDigest getDigest() {
    try {
      return MessageDigest.getInstance("SHA-512");
    } catch (NoSuchAlgorithmException e) {
      throw new RuntimeException(e);
    }
  }

  private Program<Http, String, String> call(String str) {
    return catchAll(
        pipe(
            get(baseUrl.resolve(str), ofString()),
            r -> {
              if (r.statusCode() == 200) {
                return success(r.body());
              }
              return failure("left");
            }),
        _ -> failure("left"));
  }

  private Program<Http, Void, String> callNoError(String str) {
    return catchAll(
        pipe(
            get(baseUrl.resolve(str), ofString()),
            r -> {
              if (r.statusCode() == 200) {
                return success(r.body());
              }
              return success("");
            }),
        _ -> success(""));
  }

  record TimedResponse(Instant instant, String value) {}
}
