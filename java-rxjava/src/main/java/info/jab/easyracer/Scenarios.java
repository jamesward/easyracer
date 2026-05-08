package info.jab.easyracer;

import com.sun.management.OperatingSystemMXBean;

import java.lang.management.ManagementFactory;
import java.net.URI;
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
import java.util.concurrent.CompletionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Supplier;
import java.util.stream.IntStream;

import org.jspecify.annotations.NullMarked;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.reactivex.rxjava4.core.Observable;
import io.reactivex.rxjava4.core.Single;
import io.reactivex.rxjava4.schedulers.Schedulers;

@NullMarked
public class Scenarios implements AutoCloseable {

    private static final Logger logger = LoggerFactory.getLogger(Scenarios.class);

    private static final HttpResponse.BodyHandler<String> BODY_HANDLER = HttpResponse.BodyHandlers.ofString();

    private static final ThreadFactory VIRTUAL_THREAD_FACTORY =
        Thread.ofVirtual().name("easyracer-vt-", 0).factory();

    private final URI url;
    private final ExecutorService executor;
    private final HttpClient client;

    public Scenarios(URI url) {
        this.url = url;
        this.executor = Executors.newThreadPerTaskExecutor(VIRTUAL_THREAD_FACTORY);
        this.client = HttpClient.newBuilder()
            .version(HttpClient.Version.HTTP_1_1)
            .connectTimeout(Duration.ofSeconds(5))
            // Drives async HTTP completion and dependent CF stages; avoids default FJP for many parallel requests.
            .executor(executor)
            .build();
    }

    public enum Result {
        LEFT,
        RIGHT;

        public static Result from(String body) {
            return "right".equals(body) ? RIGHT : LEFT;
        }
    }

    private HttpRequest buildGetRequest(String path) {
        return HttpRequest.newBuilder(url.resolve(path))
                .GET()
                .build();
    }

    private HttpRequest buildGetRequest(String path, Duration timeout) {
        return HttpRequest.newBuilder(url.resolve(path))
            .timeout(timeout)
            .GET()
            .build();
    }

    private Single<Result> request(String path) {
        return request(buildGetRequest(path));
    }

    private Single<Result> request(HttpRequest request) {
        return exchange(request).map(response -> response.statusCode() == 200
            ? Result.from(response.body())
            : Result.LEFT);
    }

    /**
     * Sends the request asynchronously; emits the full response (any status code) or fails on transport error.
     */
    private Single<HttpResponse<String>> exchange(HttpRequest request) {
        return Single.create(emitter -> {
            CompletableFuture<HttpResponse<String>> responseFuture =
                client.sendAsync(request, BODY_HANDLER).whenComplete((value, error) -> {
                    if (emitter.isDisposed()) {
                        return;
                    }

                    if (error != null) {
                        var cause = error instanceof CompletionException completionException
                            && completionException.getCause() != null
                                ? completionException.getCause()
                                : error;
                        emitter.onError(cause);
                    } else {
                        emitter.onSuccess(value);
                    }
                });
            emitter.setCancellable(() -> responseFuture.cancel(true));
        });
    }

    /**
     * First successful {@link Result#RIGHT} wins; if no request yields RIGHT, returns {@link Result#LEFT}.
     * Needed for scenarios where the server returns 500 on one connection before 200 "right" on another.
     */
    private static Single<Result> race(List<Single<Result>> requests) {
        return Observable.fromIterable(requests)
            .flatMap(request -> request.toObservable().onErrorComplete())
            .filter(result -> result == Result.RIGHT)
            .firstElement()
            .switchIfEmpty(Single.just(Result.LEFT));
    }

    public Single<Result> scenario1() {
        logger.info("Scenario 1");
        Supplier<Single<Result>> req = () -> request("/1");

        return race(List.of(
            req.get(), req.get()
        ));
    }

    public Single<Result> scenario2() {
        logger.info("Scenario 2");
        Supplier<Single<Result>> req = () -> request("/2");

        return race(List.of(
            req.get(), req.get()
        ));
    }

    public Single<Result> scenario3() {
        logger.info("Scenario 3");
        Supplier<Single<Result>> req = () -> request("/3");

        var requests = IntStream.range(0, 10_000)
            .mapToObj(_ -> req.get())
            .toList();
        return race(requests);
    }

    public Single<Result> scenario4() {
        logger.info("Scenario 4");
        var shortRequest = request(buildGetRequest("/4", Duration.ofSeconds(1)));
        var normalRequest = request("/4");

        return race(List.of(
            shortRequest, normalRequest
        ));
    }

    public Single<Result> scenario5() {
        logger.info("Scenario 5");
        Supplier<Single<Result>> req = () -> request("/5");

        return race(List.of(
            req.get(), req.get()
        ));
    }

    public Single<Result> scenario6() {
        logger.info("Scenario 6");
        Supplier<Single<Result>> req = () -> request("/6");
        
        return race(List.of(
            req.get(), 
            req.get(), 
            req.get()
        ));
    }

    public Single<Result> scenario7() {
        logger.info("Scenario 7");
        var hedgeDelaySeconds = 3;
        Single<Result> immediate = request("/7");
        Single<Result> hedge = Single.timer(hedgeDelaySeconds, TimeUnit.SECONDS, Schedulers.from(executor))
            .flatMap(__ -> request("/7"));
        // Hedge request often never completes HTTP; winner is whichever finishes first — typically immediate.
        return Observable.merge(
                immediate.toObservable().onErrorComplete(),
                hedge.toObservable().onErrorComplete())
            .firstElement()
            .switchIfEmpty(Single.just(Result.LEFT));
    }

    public Single<Result> scenario8() {
        logger.info("Scenario 8");

        Supplier<Single<Result>> resourceFlow = () -> {
            var openRequest = buildGetRequest("/8?open");
            return exchange(openRequest).flatMap(openResponse -> {
                if (openResponse.statusCode() != 200) {
                    return Single.just(Result.LEFT);
                }
                var resourceId = openResponse.body().trim();
                var useRequest = HttpRequest.newBuilder(url.resolve("/8?use=" + resourceId))
                    .GET()
                    .build();
                return exchange(useRequest).flatMap(useResponse -> {
                    var closeRequest = HttpRequest.newBuilder(url.resolve("/8?close=" + resourceId))
                        .GET()
                        .build();
                    return exchange(closeRequest).map(__ -> Result.RIGHT);
                });
            });
        };

        return race(List.of(
            resourceFlow.get(),
            resourceFlow.get()
        ));
    }

    public Single<Result> scenario9() {
        logger.info("Scenario 9");

        record TimedResponse(Instant instant, HttpResponse<String> response) {}

        var httpRequest = buildGetRequest("/9");
        List<Single<TimedResponse>> requests = IntStream.rangeClosed(1, 10)
            .mapToObj(index -> exchange(httpRequest).map(response -> new TimedResponse(Instant.now(), response)))
            .toList();
        Comparator<TimedResponse> byCompletionTime = Comparator.comparing(TimedResponse::instant);
        return Observable.fromIterable(requests)
            .flatMapSingle(responseSingle -> responseSingle)
            .toList()
            .map(completed ->
                completed.stream()
                    .filter(entry -> entry.response.statusCode() == 200)
                    .sorted(byCompletionTime)
                    .map(entry -> entry.response.body())
                    .reduce("", String::concat))
            .map(Result::from);
    }

    public Single<Result> scenario10() {
        logger.info("Scenario 10");
        return Single.fromCallable(this::scenario10Blocking)
            .subscribeOn(Schedulers.from(executor));
    }

    /** Blocking orchestration ported from {@code java-cf} (CPU task + blocker HTTP + load poller). */
    private Result scenario10Blocking() {
        var id = UUID.randomUUID().toString();
        var osBean = ManagementFactory.getPlatformMXBean(OperatingSystemMXBean.class);

        var cancelled = new AtomicBoolean(false);
        CompletableFuture<Void> cpuTask = CompletableFuture.runAsync(() -> {
            logger.info("scenario10 cpuTask");
            try {
                var digest = MessageDigest.getInstance("SHA-512");
                var buf = new byte[512];
                new Random().nextBytes(buf);
                while (!cancelled.get()) {
                    buf = digest.digest(buf);
                }
            } catch (NoSuchAlgorithmException e) {
                throw new RuntimeException(e);
            }
        }, executor);

        HttpRequest blockerRequest = HttpRequest.newBuilder(url.resolve("/10?" + id)).GET().build();
        CompletableFuture<HttpResponse<String>> blocker = CompletableFuture.supplyAsync(() -> {
            try {
                return client.send(blockerRequest, BODY_HANDLER);
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        }, executor).whenComplete((_response, throwable) -> {
            logger.info("scenario10 blocker");
            cancelled.set(true);
        });

        CompletableFuture<Result> loader = CompletableFuture.supplyAsync(() -> {
            logger.info("scenario10 loader");
            while (true) {
                try {
                    var load = osBean.getProcessCpuLoad() * osBean.getAvailableProcessors();
                    HttpRequest loadRequest =
                        HttpRequest.newBuilder(url.resolve("/10?" + id + "=" + load)).GET().build();
                    HttpResponse<String> response = client.send(loadRequest, BODY_HANDLER);
                    int status = response.statusCode();
                    if (status >= 200 && status < 300) {
                        return Result.from(response.body());
                    }
                    if (status >= 300 && status < 400) {
                        Thread.sleep(1000);
                    } else {
                        return Result.LEFT;
                    }
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                    return Result.LEFT;
                } catch (Exception e) {
                    return Result.LEFT;
                }
            }
        }, executor);

        var result = loader.join();
        blocker.join();
        cpuTask.join();
        return result;
    }

    public Single<Result> scenario11() {
        logger.info("Scenario 11");
        Supplier<Single<Result>> req = () -> request("/11");
        Single<Result> innerRace = race(List.of(req.get(), req.get()));
        return race(List.of(innerRace, request("/11")));
    }

    @Override
    public void close() {
        try {
            client.close();
        } finally {
            shutdownExecutor(executor);
        }
    }

    private static void shutdownExecutor(ExecutorService executor) {
        executor.shutdown();
        try {
            if (!executor.awaitTermination(30, TimeUnit.SECONDS)) {
                executor.shutdownNow();
            }
        } catch (InterruptedException e) {
            executor.shutdownNow();
            Thread.currentThread().interrupt();
        }
    }
}
