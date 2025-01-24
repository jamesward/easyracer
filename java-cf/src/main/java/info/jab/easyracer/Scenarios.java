package info.jab.easyracer;

import java.io.IOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.time.Instant;
import java.util.List;
import java.util.Objects;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Executors;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.function.Supplier;
import java.util.Comparator;
import java.util.stream.IntStream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.vavr.Function1;
import io.vavr.Function2;
import io.vavr.Function3;

public class Scenarios implements AutoCloseable {

    private static final Logger logger = LoggerFactory.getLogger(Scenarios.class);

    private static final int SHUTDOWN_TIMEOUT_SECONDS = 10;

    private final URI url;
    private final HttpClient client;
    private final HttpResponse.BodyHandler<String> config = HttpResponse.BodyHandlers.ofString();

    private final ExecutorService executorService = 
        Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors() -1);

    public Scenarios(URI url) {
        this.url = url;
        this.client = HttpClient.newBuilder()
            .executor(executorService)
            .version(HttpClient.Version.HTTP_2)
            .build();
    }

    /**
     * Enum to represent the possible values of the scenarios
     */
    public enum Values {
        LEFT("left"),
        RIGHT("right");
    
        private final String value;
    
        Values(String value) {
            this.value = value;
        }
    
        public String get() {
            return value;
        }

        public static boolean compareRight(Values value) {
            return value == RIGHT;
        }

        public static Values fromHttpResponse(HttpResponse response) {
            return fromString(response.body().toString());
        }

        public static Values fromString(String value) {
            return value.equals(RIGHT.get()) ? RIGHT : LEFT;
        }
    }

    private Function2<HttpClient, HttpRequest, CompletableFuture<Values>> asyncCall = (client, request) -> {
        return client.sendAsync(request, config)
            .orTimeout(5, TimeUnit.SECONDS)
            .handle((result, ex) -> {
                if (!Objects.isNull(ex)) {
                    logger.warn("Error occurred: " + ex.getLocalizedMessage());
                    return Values.LEFT;
                }
                if (result.statusCode() == 200) {
                    return Values.fromString(result.body());
                }
                return Values.LEFT;
            });
    };

    private Function3<Integer, HttpClient, HttpRequest, List<CompletableFuture<Values>>> getPromises = (n, client, request) -> {
        return IntStream.rangeClosed(1, n)
            .mapToObj(_ -> asyncCall.apply(client, request))
            .toList();
    };
    
    private Function1<List<CompletableFuture<Values>>, Values> process = (futures) -> {
        return futures.stream()
            .map(CompletableFuture::join)
            .filter(Values::compareRight)
            .findFirst()
            .orElse(Values.LEFT);
    };

    public Values scenario1() throws ExecutionException, InterruptedException {
        logger.info("Scenario 1");
        HttpRequest request = HttpRequest.newBuilder(url.resolve("/1")).build();

        return getPromises.andThen(process).apply(2, client, request);
    }

    public Values scenario2() throws ExecutionException, InterruptedException {
        logger.info("Scenario 2");
        HttpRequest request = HttpRequest.newBuilder(url.resolve("/2")).build();

        return getPromises.andThen(process).apply(2, client, request);
    }

    public Values scenario3() throws ExecutionException, InterruptedException, IOException {
        logger.info("Scenario 3");
        HttpRequest request = HttpRequest.newBuilder(url.resolve("/3")).build();

        //OSX issue detected when you open 10k http connections
        //https://www.tianxiangxiong.com/2024/07/08/virtual-threads.html 
        return getPromises.andThen(process).apply(10_000, client, request);
    }

    //TODO PENDING
    public Values scenario4() throws ExecutionException, InterruptedException {
        logger.info("Scenario 4");
        HttpRequest request = HttpRequest.newBuilder(url.resolve("/4")).build();

        return Values.RIGHT;
    }

    public Values scenario5() throws ExecutionException, InterruptedException {
        logger.info("Scenario 5");
        HttpRequest request = HttpRequest.newBuilder(url.resolve("/5")).build();

        return getPromises.andThen(process).apply(2, client, request);
    }

    public Values scenario6() throws ExecutionException, InterruptedException {
        logger.info("Scenario 6");
        HttpRequest request = HttpRequest.newBuilder(url.resolve("/6")).build();

        return getPromises.andThen(process).apply(3, client, request);
    }

    public Values scenario7() throws ExecutionException, InterruptedException {
        logger.info("Scenario 7");
        HttpRequest request = HttpRequest.newBuilder(url.resolve("/7")).build();

        var promise1 = client.sendAsync(request, config)
            .thenApply(Values::fromHttpResponse);
        var promise2 = CompletableFuture.supplyAsync(
                () -> client.sendAsync(request, config)
                    .thenApply(Values::fromHttpResponse)
                    .join(), 
                CompletableFuture.delayedExecutor(3, TimeUnit.SECONDS));
        
        var promises = List.of(promise1,promise2);

        return process.apply(promises);
    }

    public Values scenario8() throws ExecutionException, InterruptedException {
        logger.info("Scenario 8");

        Supplier<CompletableFuture<Values>> resourceFlow = () -> {
            record ResourceResult(String resourceId, String result) {}

            HttpRequest openRequest = HttpRequest.newBuilder(url.resolve("/8?open")).build();
            Function1<String, HttpRequest> useRequest = (resourceId) -> HttpRequest.newBuilder(url.resolve("/8?use=" + resourceId)).build();
            Function1<String, HttpRequest> closeRequest = (resourceId) -> HttpRequest.newBuilder(url.resolve("/8?close=" + resourceId)).build();

            return client.sendAsync(openRequest, config).thenApply(HttpResponse::body)
                .thenCompose(resourceId -> {
                    logger.info("id: " + resourceId);
                    return client.sendAsync(useRequest.apply(resourceId), config)
                        .thenApply(HttpResponse::body)
                        .thenApply(response -> new ResourceResult(resourceId, response));
                })
                .thenCompose(resourceResult -> {
                    logger.info("closed");
                    return client.sendAsync(closeRequest.apply(resourceResult.resourceId()), config).thenApply(_ -> Values.RIGHT);
                });
        };
        
        var promises = List.of(
            resourceFlow.get(),
            resourceFlow.get()
        );

        return process.apply(promises);
    }

    public Values scenario9() throws ExecutionException, InterruptedException {
        logger.info("Scenario 9");
        HttpRequest request = HttpRequest.newBuilder(url.resolve("/9")).build();

        record TimedResponse(Instant instant, HttpResponse<String> response) {}
        
        var promises = IntStream.rangeClosed(1, 10)
            .mapToObj(_ -> client.sendAsync(request, config).thenApply(response -> new TimedResponse(Instant.now(), response)))
            .toList();
        
        Comparator<TimedResponse> timeComparator = Comparator.comparing(TimedResponse::instant);

        var rawResult = promises.stream()
            .map(CompletableFuture::join)
            .filter(r -> r.response.statusCode() == 200)
            .sorted(timeComparator)
            .map(timedResponse -> timedResponse.response.body())
            .reduce("", String::concat);

        return Values.fromString(rawResult);
    }

    //TODO PENDING
    public Values scenario10() throws ExecutionException, InterruptedException {
        logger.info("Scenario 10");

        String id = UUID.randomUUID().toString();
        HttpRequest request = HttpRequest.newBuilder(url.resolve("/10?" + id)).build();

        return Values.RIGHT;
    }

    public Values scenario11() throws ExecutionException, InterruptedException {
        logger.info("Scenario 11");
        HttpRequest request = HttpRequest.newBuilder(url.resolve("/11")).build();

        // Create an inner race with 2 requests
        var innerRace = getPromises.andThen(process).apply(2, client, request);

        // Combine the inner race with another request
        var outerRace = List.of(
            CompletableFuture.supplyAsync(() -> innerRace),
            client.sendAsync(request, config).thenApply(Values::fromHttpResponse)
        );

        return process.apply(outerRace);
    }

    List<Values> results() throws ExecutionException, InterruptedException, IOException {
        return List.of(scenario1(), scenario2(), scenario3(), scenario4(), scenario5(), scenario6(), scenario7(), scenario8(), scenario9(), scenario10(), scenario11());
    }

    @Override
    public void close() {
        
        // First attempt a normal shutdown of the ExecutorService
        executorService.shutdown();
        try {
            if (!executorService.awaitTermination(SHUTDOWN_TIMEOUT_SECONDS, TimeUnit.SECONDS)) {
                logger.warn("Timeout reached, forcing tasks shutdown...");
                executorService.shutdownNow();
                
                if (!executorService.awaitTermination(2, TimeUnit.SECONDS)) {
                    logger.error("Could not terminate some tasks");
                }
            }
        } catch (InterruptedException e) {
            logger.error("Interrupted during ExecutorService shutdown", e);
            executorService.shutdownNow();
            Thread.currentThread().interrupt();
        }

        // Close HttpClient afterwards
        try {
            client.close();
        } catch (Exception e) {
            logger.error("Error closing HttpClient", e);
        }
    }
}
