package info.jab.easyracer;

import java.lang.management.ManagementFactory;
import java.net.URI;
import java.io.IOException;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.time.Duration;
import java.time.Instant;
import java.util.Comparator;
import java.util.List;
import java.util.Random;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.BiConsumer;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.IntStream;

import org.jspecify.annotations.NullMarked;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.sun.management.OperatingSystemMXBean;

import io.vavr.Function1;
import io.vavr.Function2;
import io.vavr.Function3;

@NullMarked
public class Scenarios implements AutoCloseable {

    private static final Logger logger = LoggerFactory.getLogger(Scenarios.class);

    private static final long CONNECT_TIMEOUT_SECONDS = 5;
    private static final ThreadFactory VIRTUAL_THREAD_FACTORY =
        Thread.ofVirtual().name("easyracer-vt-cf-", 0).factory();

    private final URI url;
    private final HttpClient client;
    private final HttpResponse.BodyHandler<String> config = HttpResponse.BodyHandlers.ofString();

    private final ExecutorService executorService = Executors.newThreadPerTaskExecutor(VIRTUAL_THREAD_FACTORY);

    public Scenarios(URI url) {
        this.url = url;
        // HTTP/2 multiplexes on few connections and is capped by SETTINGS_MAX_CONCURRENT_STREAMS; scenario 3 needs
        // ~10k requests active on the server at once or its handler never reaches the winning branch (see scenario-server).
        this.client = HttpClient.newBuilder()
            .version(HttpClient.Version.HTTP_1_1)
            .connectTimeout(Duration.ofSeconds(CONNECT_TIMEOUT_SECONDS))
            .executor(executorService)
            .build();
    }

    /**
     * Enum to represent the possible values of the scenarios
     */
    enum Value {
        LEFT("left"),
        RIGHT("right");

        private final String value;

        Value(String value) {
            this.value = value;
        }

        public String get() {
            return value;
        }

        public static boolean compareRight(Value value) {
            return value == RIGHT;
        }

        public static Value fromHttpResponse(HttpResponse<String> response) {
            return fromString(response.body());
        }

        public static Value fromString(String value) {
            return value.equals(RIGHT.get()) ? RIGHT : LEFT;
        }
    }

    //Helper methods for CompletableFuture
    
    private CompletableFuture<HttpResponse<String>> requestCompletableFutureFactory(HttpClient client, HttpRequest request) {
        return CompletableFuture.supplyAsync(() -> {
            try {
                logger.info("call handler");
                return client.send(request, config);
            } catch (IOException | InterruptedException e) {
                throw new RuntimeException(e);
            }
        }, executorService);
    }

    private CompletableFuture<HttpResponse<String>> requestCompletableFutureFactoryAsync(HttpClient client, HttpRequest request) {
        return CompletableFuture
            .runAsync(() -> logger.info("asyncCall handler"), executorService)
            .thenCompose(_ -> client.sendAsync(request, config));
    }

    private final Function2<HttpClient, HttpRequest, CompletableFuture<Value>> asyncCall =
        (client, request) -> requestCompletableFutureFactoryAsync(client, request)
            .handle((result, ex) -> {
                if (ex != null) {
                    return Value.LEFT;
                }
                if (result.statusCode() == 200) {
                    return Value.fromString(result.body());
                }
                return Value.LEFT;
            });

    @SuppressWarnings("UnusedVariable")
    private final Function2<HttpClient, HttpRequest, CompletableFuture<Value>> call = 
        (client, request) -> requestCompletableFutureFactory(client, request)
            .handle((result, ex) -> {
                if (ex != null) {
                    return Value.LEFT;
                }
                if (result.statusCode() == 200) {
                    return Value.fromString(result.body());
                }
                return Value.LEFT;
            });

    private final Function3<Integer, HttpClient, HttpRequest, List<CompletableFuture<Value>>> getPromises =
        (n, client, request) -> IntStream.rangeClosed(1, n)
            .mapToObj(_ -> call.apply(client, request))
            .toList();

    private final Function3<Integer, HttpClient, HttpRequest, List<CompletableFuture<Value>>> getPromisesAsync = (n, client,
            request) -> IntStream.rangeClosed(1, n)
                    .mapToObj(_ -> asyncCall.apply(client, request))
                    .toList();

    @SuppressWarnings("FutureReturnValueIgnored")
    private final Function1<List<CompletableFuture<Value>>, Value> race =
        (futures) -> {
            if (futures.isEmpty()) {
                return Value.LEFT;
            }
            CompletableFuture<Value> winner = new CompletableFuture<>();
            AtomicInteger remaining = new AtomicInteger(futures.size());

            Function1<CompletableFuture<Value>, BiConsumer<Value, Throwable>> callback =
                (future) -> (value, _) -> {
                    Value current;
                    if (value == null) {
                        current = Value.LEFT;
                    } else {
                        current = value;
                    }

                    if (Value.compareRight(current)) {
                        if (winner.complete(current)) {
                            futures.forEach(other -> {
                                if (other != future) {
                                    other.cancel(true);
                                }
                            });
                        }
                    } else if (remaining.decrementAndGet() == 0) {
                        winner.complete(Value.LEFT);
                    }
                };

            futures.forEach(future -> future.whenComplete(callback.apply(future)));

            return winner.join();
        };

    Value scenario1() {
        logger.info("Scenario 1");
        HttpRequest request = HttpRequest.newBuilder(url.resolve("/1")).build();

        return getPromisesAsync.andThen(race).apply(2, client, request);
    }

    Value scenario2() {
        logger.info("Scenario 2");
        HttpRequest request = HttpRequest.newBuilder(url.resolve("/2")).build();

        return getPromisesAsync.andThen(race).apply(2, client, request);
    }

    Value scenario3() {
        logger.info("Scenario 3");
        HttpRequest request = HttpRequest.newBuilder(url.resolve("/3")).build();

        //OSX issue detected when you open 10k http connections
        //https://www.tianxiangxiong.com/2024/07/08/virtual-threads.html
        return getPromisesAsync.andThen(race).apply(10_000, client, request);
    }

    Value scenario4() {
        logger.info("Scenario 4");
        // Timeout set on the HttpRequest so the underlying connection is actually
        // closed on timeout (server only releases the winner once the loser is gone).
        HttpRequest shortRequest = HttpRequest.newBuilder(url.resolve("/4"))
            .timeout(Duration.ofSeconds(1))
            .build();
        HttpRequest request = HttpRequest.newBuilder(url.resolve("/4")).build();

        var promises = List.of(
            asyncCall.apply(client, shortRequest),
            asyncCall.apply(client, request)
        );

        return race.apply(promises);
    }

    Value scenario5() {
        logger.info("Scenario 5");
        HttpRequest request = HttpRequest.newBuilder(url.resolve("/5")).build();

        return getPromisesAsync.andThen(race).apply(2, client, request);
    }

    Value scenario6() {
        logger.info("Scenario 6");
        HttpRequest request = HttpRequest.newBuilder(url.resolve("/6")).build();

        return getPromisesAsync.andThen(race).apply(3, client, request);
    }

    Value scenario7() {
        logger.info("Scenario 7");
        var timeoutSeconds = 3;
        HttpRequest request = HttpRequest.newBuilder(url.resolve("/7")).build();

        var promise1 = requestCompletableFutureFactoryAsync(client, request)
            .thenApply(response -> {
                return Value.fromHttpResponse(response);
            });
        var promise2 = CompletableFuture.supplyAsync(
                () -> requestCompletableFutureFactoryAsync(client, request),
                CompletableFuture.delayedExecutor(timeoutSeconds, TimeUnit.SECONDS, executorService))
            .thenCompose(Function.identity())
            .thenApply(Value::fromHttpResponse);

        var promises = List.of(promise1,promise2);

        return race.apply(promises);
    }

    Value scenario8() {
        logger.info("Scenario 8");

        Supplier<CompletableFuture<Value>> resourceFlow = () -> {
            record ResourceResult(String resourceId, String result) {}

            HttpRequest openRequest = HttpRequest.newBuilder(url.resolve("/8?open")).build();
            Function1<String, HttpRequest> useRequest = (resourceId) -> HttpRequest.newBuilder(url.resolve("/8?use=" + resourceId)).build();
            Function1<String, HttpRequest> closeRequest = (resourceId) -> HttpRequest.newBuilder(url.resolve("/8?close=" + resourceId)).build();

            return requestCompletableFutureFactoryAsync(client, openRequest).thenApply(HttpResponse::body)
                .thenCompose(resourceId -> {
                    logger.info("id: {}", resourceId);
                    return requestCompletableFutureFactoryAsync(client, useRequest.apply(resourceId))
                        .thenApply(HttpResponse::body)
                        .thenApply(response -> new ResourceResult(resourceId, response));
                })
                .thenCompose(resourceResult -> {
                    logger.info("closed");
                    return requestCompletableFutureFactoryAsync(client, closeRequest.apply(resourceResult.resourceId()))
                        .thenApply(_ -> Value.RIGHT);
                });
        };

        var promises = List.of(
            resourceFlow.get(),
            resourceFlow.get()
        );

        return race.apply(promises);
    }

    Value scenario9() {
        logger.info("Scenario 9");
        HttpRequest request = HttpRequest.newBuilder(url.resolve("/9")).build();

        record TimedResponse(Instant instant, HttpResponse<String> response) {}

        var promises = IntStream.rangeClosed(1, 10)
            .mapToObj(index -> requestCompletableFutureFactoryAsync(client, request)
                .thenApply(response -> {
                    return new TimedResponse(Instant.now(), response);
                }))
            .toList();

        Comparator<TimedResponse> timeComparator = Comparator.comparing(TimedResponse::instant);

        var rawResult = promises.stream()
            .map(CompletableFuture::join)
            .filter(r -> r.response.statusCode() == 200)
            .sorted(timeComparator)
            .map(timedResponse -> timedResponse.response.body())
            .reduce("", String::concat);

        return Value.fromString(rawResult);
    }

    Value scenario10() {
        logger.info("Scenario 10");
        var id = UUID.randomUUID().toString();
        var osBean = ManagementFactory.getPlatformMXBean(OperatingSystemMXBean.class);

        // CPU-heavy task: SHA-512 digest loop on a virtual thread.
        // CompletableFuture.cancel(true) does NOT propagate interrupts, so we use a
        // cooperative AtomicBoolean flag flipped by the blocker callback.
        var cancelled = new AtomicBoolean(false);
        CompletableFuture<Void> cpuTask = CompletableFuture.runAsync(() -> {
            logger.info("scenario10 cpuTask");
            try {
                var messageDigest = MessageDigest.getInstance("SHA-512");
                var buf = new byte[512];
                new Random().nextBytes(buf);
                while (!cancelled.get()) {
                    buf = messageDigest.digest(buf);
                }
            } catch (NoSuchAlgorithmException e) {
                throw new RuntimeException(e);
            }
        }, executorService);

        // Part 1 (blocker): hold the HTTP connection open; signal the CPU task to stop once it closes.
        HttpRequest blockerRequest = HttpRequest.newBuilder(url.resolve("/10?" + id)).build();
        var blocker = requestCompletableFutureFactoryAsync(client, blockerRequest)
            .whenComplete((response, ex) -> {
                cancelled.set(true);
            });

        // Part 2 (load poller): every 1s send the current process CPU load.
        // 2xx -> done, 3xx -> keep polling, 4xx -> error.
        var loader = CompletableFuture.supplyAsync(() -> {
            logger.info("scenario10 loader");
            while (true) {
                try {
                    var load = osBean.getProcessCpuLoad() * osBean.getAvailableProcessors();
                    HttpRequest request = HttpRequest.newBuilder(url.resolve("/10?" + id + "=" + load)).build();
                    HttpResponse<String> response = client.send(request, config);
                    int status = response.statusCode();
                    if (status >= 200 && status < 300) {
                        return Value.fromString(response.body());
                    } else if (status >= 300 && status < 400) {
                        Thread.sleep(1000);
                    } else {
                        return Value.LEFT;
                    }
                } catch (Exception e) {
                    return Value.LEFT;
                }
            }
        }, executorService);

        var result = loader.join();
        blocker.join();
        cpuTask.join();
        return result;
    }

    Value scenario11() {
        logger.info("Scenario 11");
        HttpRequest request = HttpRequest.newBuilder(url.resolve("/11")).build();

        // Server unblocks when three connections are open; the inner pair and the third must overlap.
        CompletableFuture<Value> innerRace = CompletableFuture.supplyAsync(
            () -> getPromisesAsync.andThen(race).apply(2, client, request),
            executorService);
        CompletableFuture<Value> third =
            requestCompletableFutureFactoryAsync(client, request).thenApply(Value::fromHttpResponse);

        return race.apply(List.of(innerRace, third));
    }

    @Override
    public void close() {
        executorService.shutdown();
        try {
            if (!executorService.awaitTermination(5, TimeUnit.SECONDS)) {
                executorService.shutdownNow();
            }
        } catch (InterruptedException e) {
            executorService.shutdownNow();
            Thread.currentThread().interrupt();
        }
    }
}
