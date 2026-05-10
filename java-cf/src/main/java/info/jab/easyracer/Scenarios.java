package info.jab.easyracer;

import module java.base;
import module java.net.http;
import module java.management;

import com.sun.management.OperatingSystemMXBean;

import org.jspecify.annotations.NullMarked;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@NullMarked
public class Scenarios implements AutoCloseable {

    private static final Logger logger = LoggerFactory.getLogger(Scenarios.class);

    //Virtual threads 
    private static final long CONNECT_TIMEOUT_SECONDS = 5;
    private static final ThreadFactory VIRTUAL_THREAD_FACTORY =
        Thread.ofVirtual().name("easyracer-vt-cf-", 0).factory();
    private final ExecutorService executorService = Executors.newThreadPerTaskExecutor(VIRTUAL_THREAD_FACTORY);

    //HTTP client
    private final URI url;
    private final HttpClient client;
    private final HttpResponse.BodyHandler<String> config = HttpResponse.BodyHandlers.ofString();

    public Scenarios(URI url) {
        this.url = url;
        // HTTP/2 multiplexes on few connections and is capped by SETTINGS_MAX_CONCURRENT_STREAMS; scenario 3 needs
        // ~10k requests active on the server at once or its handler never reaches the winning branch (see scenario-server).
        this.client = httpClient(executorService, CONNECT_TIMEOUT_SECONDS);
    }

    /**
     * Result of a scenario run (response body {@code left} or {@code right}).
     */
    enum ScenarioResult {
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
    
    private CompletableFuture<HttpResponse<String>> sendAsync(HttpClient client, HttpRequest request) {
        return client.sendAsync(request, config);
    }

    private HttpClient httpClient(Executor executor, long connectTimeoutSeconds) {
        return HttpClient.newBuilder()
            .version(HttpClient.Version.HTTP_1_1)
            .connectTimeout(Duration.ofSeconds(connectTimeoutSeconds))
            .executor(executor)
            .build();
    }

    private void sleep(long seconds) {
        try {
            Thread.sleep(seconds * 1000);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    }

    private HttpRequest request(String path) {
        return HttpRequest.newBuilder(url.resolve(path)).build();
    }

    private CompletableFuture<ScenarioResult> sendNonBlocking(HttpClient client, HttpRequest request) {
        return sendAsync(client, request)
            .thenApplyAsync(response -> ScenarioResult.fromString(response.body()), executorService)
            .exceptionally(_ -> ScenarioResult.LEFT);
    }

    private List<CompletableFuture<ScenarioResult>> getPromisesAsync(int n, HttpClient client, HttpRequest request) {
        return IntStream.rangeClosed(1, n)
            .mapToObj(_ -> sendNonBlocking(client, request))
            .toList();
    }

    @SuppressWarnings("FutureReturnValueIgnored")
    private ScenarioResult race(List<CompletableFuture<ScenarioResult>> promises) {
        if (promises.isEmpty()) {
            return ScenarioResult.LEFT;
        }

        CompletableFuture<ScenarioResult> winner = new CompletableFuture<>();
        AtomicInteger remaining = new AtomicInteger(promises.size());

        for (CompletableFuture<ScenarioResult> promise : promises) {
            promise.whenComplete((value, throwable) -> {
                ScenarioResult result = throwable != null || value == null
                        ? ScenarioResult.LEFT
                        : value;
                if (result == ScenarioResult.RIGHT) {
                    boolean won = winner.complete(ScenarioResult.RIGHT);
                    if (won) {
                        promises.forEach(otherPromise -> {
                            if (otherPromise != promise) {
                                otherPromise.cancel(true);
                            }
                        });
                    }
                } else if (remaining.decrementAndGet() == 0) {
                    winner.complete(ScenarioResult.LEFT);
                }
            });
        }

        return winner.join();
    }

    //Scenarios

    ScenarioResult scenario1() {
        logger.info("Scenario 1");
        HttpRequest request = request("/1");

        return race(List.of(
                sendNonBlocking(client, request),
                sendNonBlocking(client, request)));
    }

    ScenarioResult scenario2() {
        logger.info("Scenario 2");
        HttpRequest request = request("/2");

        return race(List.of(
                sendNonBlocking(client, request),
                sendNonBlocking(client, request)));
    }

    ScenarioResult scenario3() {
        logger.info("Scenario 3");
        HttpRequest request = request("/3");

        // OSX issue detected when you open 10k http connections
        // https://www.tianxiangxiong.com/2024/07/08/virtual-threads.html
        // In OSX, it is possible to solve the scenario using a Dynamic Headroom approach.
        return race(getPromisesAsync(10_000, client, request));
    }

    ScenarioResult scenario4() {
        logger.info("Scenario 4");
        HttpRequest shortRequest = HttpRequest.newBuilder(url.resolve("/4"))
            .timeout(Duration.ofSeconds(1))
            .build();
        HttpRequest request = request("/4");

        return race(List.of(
                sendNonBlocking(client, shortRequest),
                sendNonBlocking(client, request)));
    }

    ScenarioResult scenario5() {
        logger.info("Scenario 5");
        HttpRequest request = request("/5");

        return race(List.of(
                sendNonBlocking(client, request),
                sendNonBlocking(client, request)));
    }

    ScenarioResult scenario6() {
        logger.info("Scenario 6");
        HttpRequest request = request("/6");

        return race(List.of(
                sendNonBlocking(client, request),
                sendNonBlocking(client, request),
                sendNonBlocking(client, request)));
    }

    ScenarioResult scenario7() {
        logger.info("Scenario 7");

        var timeoutSeconds = 3;
        HttpRequest request = request("/7");

        CompletableFuture<ScenarioResult> immediate = sendNonBlocking(client, request);

        CompletableFuture<ScenarioResult> delayed = CompletableFuture.supplyAsync(() -> {
                    sleep(timeoutSeconds);
                    return null;
                }, executorService)
                .thenCompose(_ -> sendNonBlocking(client, request));

        return race(List.of(immediate, delayed));
    }

    ScenarioResult scenario8() {
        logger.info("Scenario 8");

        HttpRequest openRequest = request("/8?open");
        Function<String, HttpRequest> useRequest = id -> request("/8?use=" + id);
        Function<String, HttpRequest> closeRequest = id -> request("/8?close=" + id);

        Supplier<CompletableFuture<ScenarioResult>> flowSupplier = () -> sendAsync(client, openRequest)
            .thenApplyAsync(HttpResponse::body, executorService)
            .thenComposeAsync(resourceId -> sendAsync(client, useRequest.apply(resourceId))
                .thenApplyAsync(HttpResponse::body, executorService)
                .thenComposeAsync(ignored -> sendAsync(client, closeRequest.apply(resourceId))
                    .thenApplyAsync(ignoredResponse -> ScenarioResult.RIGHT, executorService), executorService), executorService);

        return race(List.of(flowSupplier.get(), flowSupplier.get()));
    }

    ScenarioResult scenario9() {
        logger.info("Scenario 9");

        HttpRequest request = request("/9");

        record TimedResponse(Instant instant, HttpResponse<String> response) { }

        var promises = IntStream.rangeClosed(1, 10)
                .mapToObj(_ -> sendAsync(client, request)
                        .thenApplyAsync(response -> new TimedResponse(Instant.now(), response), executorService))
                .toList();

        Comparator<TimedResponse> timeComparator = Comparator.comparing(TimedResponse::instant);

        var rawResult = promises.stream()
                .map(CompletableFuture::join)
                .filter(r -> r.response.statusCode() == 200)
                .sorted(timeComparator)
                .map(timedResponse -> timedResponse.response.body())
                .reduce("", String::concat);

        return ScenarioResult.fromString(rawResult);
    }

    final class CpuTask {

        private final ExecutorService executor;
        private final AtomicBoolean cancelled = new AtomicBoolean(false);

        CpuTask(ExecutorService executor) {
            this.executor = executor;
        }

        CompletableFuture<Void> start() {
            return CompletableFuture.runAsync(() -> {
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
        }

        void stop() {
            cancelled.set(true);
        }
    }

    final class CpuLoadPoller {

        private final HttpClient client;
        private final HttpResponse.BodyHandler<String> config;
        private final Executor executor;

        CpuLoadPoller(HttpClient client,
                HttpResponse.BodyHandler<String> config,
                Executor executor) {
            this.client = client;
            this.config = config;
            this.executor = executor;
        }

        CompletableFuture<ScenarioResult> poll(OperatingSystemMXBean osBean, String id) {

            var load = osBean.getProcessCpuLoad() * osBean.getAvailableProcessors();
            HttpRequest request = Scenarios.this.request("/10?" + id + "=" + load);

            return client.sendAsync(request, config)
                    .thenComposeAsync(response -> {
                        int status = response.statusCode();

                        if (status >= 200 && status < 300) {
                            return CompletableFuture.completedFuture(
                                    ScenarioResult.fromString(response.body()));
                        }

                        if (status >= 300 && status < 400) {
                            return CompletableFuture
                                    .runAsync(() -> sleep(1), executor)
                                    .thenCompose(_ -> poll(osBean, id));
                        }

                        return CompletableFuture.completedFuture(ScenarioResult.LEFT);
                    }, executor)
                    .exceptionally(_ -> ScenarioResult.LEFT);
        }
    }

    ScenarioResult scenario10() {
        logger.info("Scenario 10");

        var id = UUID.randomUUID().toString();
        var osBean = ManagementFactory.getPlatformMXBean(OperatingSystemMXBean.class);

        var cpuTask = new CpuTask(executorService);
        var poller = new CpuLoadPoller(client, config, executorService);

        CompletableFuture<Void> cpu = cpuTask.start();

        CompletableFuture<HttpResponse<String>> blocker = sendAsync(client, request("/10?" + id))
                .whenCompleteAsync((_, _) -> cpuTask.stop(), executorService);

        CompletableFuture<ScenarioResult> result = poller.poll(osBean, id);

        var value = result.join();

        blocker.join();
        cpu.join();

        return value;
    }

    ScenarioResult scenario11() {
        logger.info("Scenario 11");

        HttpRequest request = request("/11");

        // inner pair
        List<CompletableFuture<ScenarioResult>> inner = getPromisesAsync(2, client, request);

        // third request
        CompletableFuture<ScenarioResult> third = sendAsync(client, request)
                .thenApplyAsync(response -> ScenarioResult.fromString(response.body()), executorService);

        // combine all directly
        return race(List.of(
                inner.get(0),
                inner.get(1),
                third));
    }

    @Override
    public void close() {
        executorService.shutdown();
        try {
            executorService.awaitTermination(10, TimeUnit.SECONDS);
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        } finally {
            executorService.shutdownNow();
        }
    }
}
