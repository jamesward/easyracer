package info.jab.easyracer;

import module java.base;
import module java.net.http;
import module java.management;

import org.jspecify.annotations.NullMarked;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.reactivex.rxjava4.core.Completable;
import io.reactivex.rxjava4.core.Maybe;
import io.reactivex.rxjava4.core.Observable;
import io.reactivex.rxjava4.core.Scheduler;
import io.reactivex.rxjava4.core.Single;
import io.reactivex.rxjava4.disposables.CompositeDisposable;
import io.reactivex.rxjava4.disposables.Disposable;
import io.reactivex.rxjava4.schedulers.Schedulers;

@NullMarked
public class Scenarios implements AutoCloseable {

    private static final Logger logger = LoggerFactory.getLogger(Scenarios.class);

    private static final HttpResponse.BodyHandler<String> BODY_HANDLER = HttpResponse.BodyHandlers.ofString();

    private static final ThreadFactory VIRTUAL_THREAD_FACTORY =
        Thread.ofVirtual().name("easyracer-vt-", 0).factory();

    private final URI url;
    private final ExecutorService executor;
    private final Scheduler scheduler;
    private final HttpClient client;

    public Scenarios(URI url) {
        this.url = url;
        this.executor = Executors.newThreadPerTaskExecutor(VIRTUAL_THREAD_FACTORY);
        this.scheduler = Schedulers.from(executor);
        this.client = HttpClient.newBuilder()
            .version(HttpClient.Version.HTTP_1_1)
            .connectTimeout(Duration.ofSeconds(5))
            // Drives async HTTP completion and dependent CF stages; avoids default FJP for many parallel requests.
            .executor(executor)
            .build();
    }

    /**
     * Result of a scenario run (response body {@code left} or {@code right}).
     */
    public enum ScenarioResult {
        LEFT,
        RIGHT;

        public static ScenarioResult fromString(String value) {
            return switch (value) {
                case "left" -> LEFT;
                case "right" -> RIGHT;
                default -> {
                    logger.warn("Invalid value '{}', returning LEFT", value);
                    yield LEFT;
                }
            };
        }
    }

    //Helper methods

    private Single<ScenarioResult> request(String path) {
        var httpRequest = HttpRequest.newBuilder(url.resolve(path))
            .GET()
            .build();
        return toScenarioResult(exchange(httpRequest));
    }

    private Single<ScenarioResult> request(String path, Duration timeout) {
        var httpRequest = HttpRequest.newBuilder(url.resolve(path))
            .timeout(timeout)
            .GET()
            .build();
        return toScenarioResult(exchange(httpRequest));
    }

    private static Single<ScenarioResult> toScenarioResult(Single<HttpResponse<String>> response) {
        return response.map(r -> r.statusCode() == 200
            ? ScenarioResult.fromString(r.body())
            : ScenarioResult.LEFT);
    }

    /**
     * Sends the request asynchronously; emits the full response (any status code) or fails on transport error.
     */
    private Single<HttpResponse<String>> exchange(HttpRequest request) {
        return Single.defer(() -> {
            CompletableFuture<HttpResponse<String>> future =
                client.sendAsync(request, BODY_HANDLER);

            return Single.fromCompletionStage(future)
                .doOnDispose(() -> future.cancel(true));
        }).onErrorResumeNext(error -> Single.error(
            error instanceof CompletionException ce && ce.getCause() != null
                ? ce.getCause()
                : error));
    }

    /**
     * First successful {@link ScenarioResult#RIGHT} wins; if no request yields RIGHT, returns {@link ScenarioResult#LEFT}.
     * Needed for scenarios where the server returns 500 on one connection before 200 "right" on another.
     */
    @SafeVarargs
    @SuppressWarnings({"unchecked", "varargs"})
    private static Single<ScenarioResult> race(Single<ScenarioResult>... requests) {
        return Observable.mergeArrayDelayError(
                Arrays.stream(requests)
                    .map(req -> req
                        .onErrorReturnItem(ScenarioResult.LEFT)
                        .toObservable())
                    .toArray(Observable[]::new))
            .filter(result -> result == ScenarioResult.RIGHT)
            .first(ScenarioResult.LEFT);
    }

    //Scenarios
    
    public Single<ScenarioResult> scenario1() {
        logger.info("Scenario 1");
        Supplier<Single<ScenarioResult>> req = () -> request("/1");

        return race(
            req.get(), 
            req.get());
    }

    public Single<ScenarioResult> scenario2() {
        logger.info("Scenario 2");
        Supplier<Single<ScenarioResult>> req = () -> request("/2");

        return race(
            req.get(), 
            req.get());
    }

    @SuppressWarnings("unchecked")
    public Single<ScenarioResult> scenario3() {
        logger.info("Scenario 3");
        Supplier<Single<ScenarioResult>> req = () -> request("/3");

        Single<ScenarioResult>[] requests = IntStream.range(0, 10_000)
            .mapToObj(_ -> req.get())
            .toArray(Single[]::new);

        return race(requests);
    }

    public Single<ScenarioResult> scenario4() {
        logger.info("Scenario 4");
        var shortRequest = request("/4", Duration.ofSeconds(1));
        var normalRequest = request("/4");

        return race(
            shortRequest, 
            normalRequest);
    }

    public Single<ScenarioResult> scenario5() {
        logger.info("Scenario 5");
        Supplier<Single<ScenarioResult>> req = () -> request("/5");

        return race(
            req.get(), 
            req.get());
    }

    public Single<ScenarioResult> scenario6() {
        logger.info("Scenario 6");
        Supplier<Single<ScenarioResult>> req = () -> request("/6");
        
        return race(
            req.get(), 
            req.get(), 
            req.get());
    }

    public Single<ScenarioResult> scenario7() {
        logger.info("Scenario 7");
        var hedgeDelaySeconds = 3;
        Single<ScenarioResult> immediate = request("/7");
        Single<ScenarioResult> hedge = Single.timer(hedgeDelaySeconds, TimeUnit.SECONDS, scheduler)
            .flatMap(__ -> request("/7"));
        // Hedge request often never completes HTTP; first successful response wins — typically immediate.
        return Single.ambArray(
                immediate.onErrorResumeWith(Single.never()),
                hedge.onErrorResumeWith(Single.never()))
            .onErrorReturnItem(ScenarioResult.LEFT);
    }

    public Single<ScenarioResult> scenario8() {
        logger.info("Scenario 8");

        Supplier<Single<ScenarioResult>> resourceFlow = () -> {
            var openRequest = HttpRequest.newBuilder(url.resolve("/8?open")).GET().build();
            return exchange(openRequest).flatMap(openResponse -> {
                if (openResponse.statusCode() != 200) {
                    return Single.just(ScenarioResult.LEFT);
                }
                var resourceId = openResponse.body().trim();
                return request("/8?use=" + resourceId).flatMap(__ ->
                    exchange(HttpRequest.newBuilder(url.resolve("/8?close=" + resourceId)).GET().build())
                        .map(___ -> ScenarioResult.RIGHT));
            });
        };

        return race(
            resourceFlow.get(), 
            resourceFlow.get());
    }

    public Single<ScenarioResult> scenario9() {
        logger.info("Scenario 9");

        record TimedResponse(Instant instant, HttpResponse<String> response) {}

        var httpRequest = HttpRequest.newBuilder(url.resolve("/9")).GET().build();
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
            .map(ScenarioResult::fromString);
    }

    public Single<ScenarioResult> scenario10() {
        logger.info("Scenario 10");
        var id = UUID.randomUUID().toString();
        var availableProcessors = Runtime.getRuntime().availableProcessors();

        // CPU-burning task: hashes a buffer until the surrounding subscription is disposed.
        Completable cpuTask = Completable.create(emitter -> {
            logger.info("scenario10 cpuTask");
            var digest = MessageDigest.getInstance("SHA-512");
            var buf = new byte[512];
            new Random().nextBytes(buf);
            while (!emitter.isDisposed()) {
                buf = digest.digest(buf);
            }
            emitter.onComplete();
        }).subscribeOn(scheduler);

        // Server-held request that signals "test in progress"; resolves when the server is satisfied.
        Completable blocker = exchange(HttpRequest.newBuilder(url.resolve("/10?" + id)).GET().build())
            .doOnSuccess(__ -> logger.info("scenario10 blocker"))
            .ignoreElement();

        // Reports the measured CPU load once per second, retrying on 3xx until success or terminal status.
        Single<ScenarioResult> loader = Single
            .defer(() -> {
                double load = processCpuLoadFraction() * availableProcessors;
                return exchange(HttpRequest.newBuilder(url.resolve("/10?" + id + "=" + load)).GET().build());
            })
            .flatMapMaybe(response -> {
                int status = response.statusCode();
                if (status >= 200 && status < 300) {
                    return Maybe.just(ScenarioResult.fromString(response.body()));
                }
                if (status >= 300 && status < 400) {
                    // empty -> repeatWhen schedules another poll after the delay
                    return Maybe.empty();
                }
                return Maybe.just(ScenarioResult.LEFT);
            })
            .toObservable()
            .repeatWhen(completions -> completions.delay(1, TimeUnit.SECONDS, scheduler))
            .firstOrError()
            .doOnSubscribe(__ -> logger.info("scenario10 loader"))
            .onErrorReturnItem(ScenarioResult.LEFT);

        // Start CPU loop and blocker concurrently; the blocker's completion stops the CPU loop so the loader can observe the drop.
        return Single.using(
            () -> {
                Disposable cpu = cpuTask.subscribe(
                    () -> {},
                    error -> logger.warn("scenario10 cpuTask error", error));
                Disposable blockerSub = blocker.subscribe(
                    cpu::dispose,
                    error -> {
                        logger.warn("scenario10 blocker error", error);
                        cpu.dispose();
                    });
                return new CompositeDisposable(cpu, blockerSub);
            },
            __ -> loader,
            Disposable::dispose);
    }

    /** JVM process CPU usage in {@code [0, 1]} from {@code java.lang:type=OperatingSystem}, or {@code 0} if absent. */
    private static double processCpuLoadFraction() {
        try {
            var mbeanServer = ManagementFactory.getPlatformMBeanServer();
            var objectName = ObjectName.getInstance("java.lang:type=OperatingSystem");
            var value = mbeanServer.getAttribute(objectName, "ProcessCpuLoad");
            if (value instanceof Double d && !d.isNaN() && d >= 0) {
                return d;
            }
        } catch (JMException | ClassCastException e) {
            logger.debug("ProcessCpuLoad not available: {}", e.toString());
        }
        return 0.0;
    }

    public Single<ScenarioResult> scenario11() {
        logger.info("Scenario 11");
        var httpRequest = HttpRequest.newBuilder(url.resolve("/11")).GET().build();
        
        Supplier<Single<ScenarioResult>> req = () -> toScenarioResult(exchange(httpRequest));
        Single<ScenarioResult> innerRace = race(req.get(), req.get());
        return race(req.get(), innerRace);
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
