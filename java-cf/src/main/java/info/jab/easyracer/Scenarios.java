package info.jab.easyracer;

import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.time.Instant;
import java.util.List;
import java.util.Objects;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.StructuredTaskScope;
import java.io.IOException;
import java.util.concurrent.TimeUnit;
import java.util.function.Supplier;
import java.util.Comparator;
import java.util.stream.Gatherers;
import java.util.stream.IntStream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.vavr.Function1;
import io.vavr.Function2;
import io.vavr.Function3;

public class Scenarios {

    private static final Logger logger = LoggerFactory.getLogger(Scenarios.class);

    private final URI url;
    private final HttpClient client;
    private final HttpResponse.BodyHandler<String> config = HttpResponse.BodyHandlers.ofString();

    public Scenarios(URI url) {
        this.url = url;
        this.client = HttpClient.newHttpClient();
    }

    private Function2<HttpClient, HttpRequest, CompletableFuture<String>> asyncCall = (client, request) -> {
        return client.sendAsync(request, config)
            .orTimeout(5, TimeUnit.SECONDS)
            .handle((result, ex) -> {
                if (!Objects.isNull(ex)) {
                    logger.warn("Error occurred: " + ex.getLocalizedMessage());
                    return "left";
                }
                if (result.statusCode() == 200) {
                    return result.body();
                }
                return "left";
            });
    };

    private Function3<Integer, HttpClient, HttpRequest, List<CompletableFuture<String>>> getPromises = (n, client, request) -> {
        return IntStream.rangeClosed(0, n)
            .mapToObj(_ -> asyncCall.apply(client, request))
            .toList();
    };

    private Function1<List<CompletableFuture<String>>, String> process = (futures) -> {
        return futures.stream()
            .map(CompletableFuture::join)
            .filter(str -> str.equals("right"))
            .findFirst()
            .orElse("left");
    };
    
    public String scenario1() throws ExecutionException, InterruptedException {
        logger.info("Scenario 1");
        HttpRequest request = HttpRequest.newBuilder(url.resolve("/1")).build();

        return getPromises.andThen(process).apply(2, client, request);
    }

    public String scenario2() throws ExecutionException, InterruptedException {
        logger.info("Scenario 2");
        HttpRequest request = HttpRequest.newBuilder(url.resolve("/2")).build();

        return getPromises.andThen(process).apply(2, client, request);
    }

    private String scenario3Original() throws ExecutionException, InterruptedException, IOException {
        logger.info("Scenario 3");
        HttpRequest request = HttpRequest.newBuilder(url.resolve("/3")).build();

        var futures = IntStream.range(1, 10_000)
            .mapToObj(_ -> asyncCall.apply(client, request))
            .toList();

        return CompletableFuture.anyOf(futures.toArray(CompletableFuture[]::new))
            .thenApply(response -> (String) response)
            .join();
    }

    private String scenario3Streams() throws ExecutionException, InterruptedException, IOException {
        logger.info("Scenario 3");
        HttpRequest request = HttpRequest.newBuilder(url.resolve("/3")).build();

        var futures = IntStream.range(1, 10_000)
            .mapToObj(_ -> asyncCall.apply(client, request))
            .toList();

        return futures.stream()
            .map(CompletableFuture::join)
            .filter(str -> str.equals("right"))
            .findFirst()
            .orElse("right");//TODO WIP, it should be left
    }

    private String scenarioGatherers() throws ExecutionException, InterruptedException, IOException {
        logger.info("Scenario 3");
        HttpRequest request = HttpRequest.newBuilder(url.resolve("/3")).build();

        return IntStream.rangeClosed(1, 10_000).boxed()
            .gather(Gatherers.mapConcurrent(10_000, _ -> asyncCall.apply(client, request).join()))
            .filter(cf -> cf.equals("right"))
            .findFirst()
            .orElse("left");
    }

    //TODO PENDING
    public String scenario3() throws ExecutionException, InterruptedException, IOException {
        //return scenario3Original();
        //return scenario3Streams();
        return scenarioGatherers();
        
        logger.info("Scenario 3");
        HttpRequest request = HttpRequest.newBuilder(url.resolve("/3")).build();

        try (var scope = new StructuredTaskScope.ShutdownOnSuccess<HttpResponse<String>>()) {
            IntStream.rangeClosed(1, 10_000)
                    .forEach(i ->
                            scope.fork(() ->
                                    client.send(request, config)
                            )
                    );
            scope.join();
            return scope.result().body();
        }
    }

    //TODO PENDING
    public String scenario4() throws ExecutionException, InterruptedException {
        logger.info("Scenario 4");
        HttpRequest request = HttpRequest.newBuilder(url.resolve("/4")).build();

        return "right";
    }

    public String scenario5() throws ExecutionException, InterruptedException {
        logger.info("Scenario 5");
        HttpRequest request = HttpRequest.newBuilder(url.resolve("/5")).build();

        return getPromises.andThen(process).apply(2, client, request);
    }

    public String scenario6() throws ExecutionException, InterruptedException {
        logger.info("Scenario 6");
        HttpRequest request = HttpRequest.newBuilder(url.resolve("/6")).build();

        return getPromises.andThen(process).apply(3, client, request);
    }

    public String scenario7() throws ExecutionException, InterruptedException {
        logger.info("Scenario 7");
        HttpRequest request = HttpRequest.newBuilder(url.resolve("/7")).build();

        var future1 = client.sendAsync(request, config);
        var future2 = CompletableFuture.supplyAsync(
                () -> client.sendAsync(request, config).join(), 
                CompletableFuture.delayedExecutor(3, TimeUnit.SECONDS));
        
        return CompletableFuture.anyOf(
            future1.thenApply(HttpResponse::body), 
            future2.thenApply(HttpResponse::body))
        .thenApply(response -> (String) response)
        .join();
    }

    public String scenario8() throws ExecutionException, InterruptedException {
        logger.info("Scenario 8");

        Supplier<CompletableFuture<String>> resourceFlow = () -> {
            record ResourceResult(String resourceId, String result) {}

            HttpRequest openRequest = HttpRequest.newBuilder(url.resolve("/8?open")).build();
            Function1<String, HttpRequest> useRequest = (resourceId) -> HttpRequest.newBuilder(url.resolve("/8?use=" + resourceId)).build();
            Function1<String, HttpRequest> closeRequest = (resourceId) -> HttpRequest.newBuilder(url.resolve("/8?close=" + resourceId)).build();

            return asyncCall.apply(client, openRequest)
                .thenCompose(resourceId -> {
                    logger.info("id: " + resourceId);
                    return asyncCall.apply(client, useRequest.apply(resourceId))
                        .thenApply(response -> new ResourceResult(resourceId, response));
                })
                .thenCompose(resourceResult -> {
                    logger.info("closed");
                    return asyncCall.apply(client, closeRequest.apply(resourceResult.resourceId())).thenApply(_ -> "right");
                });
        };
        
        var promises = List.of(
            resourceFlow.get(),
            resourceFlow.get()
        );

        return process.apply(promises);
    }

    public String scenario9() throws ExecutionException, InterruptedException {
        logger.info("Scenario 9");
        HttpRequest request = HttpRequest.newBuilder(url.resolve("/9")).build();

        record TimedResponse(Instant instant, HttpResponse<String> response) {}
        
        var futures = IntStream.range(0, 10)
            .mapToObj(_ -> client.sendAsync(request, config).thenApply(response -> new TimedResponse(Instant.now(), response)))
            .toList();
        
        Comparator<TimedResponse> timeComparator = Comparator.comparing(TimedResponse::instant);

        return futures.stream()
            .map(CompletableFuture::join)
            .filter(r -> r.response.statusCode() == 200)
            .sorted(timeComparator)
            .map(timedResponse -> timedResponse.response.body())
            .reduce("", String::concat);
    }

    //TODO PENDING
    public String scenario10() throws ExecutionException, InterruptedException {
        logger.info("Scenario 10");

        String id = UUID.randomUUID().toString();
        HttpRequest request = HttpRequest.newBuilder(url.resolve("/10?" + id)).build();

        return "right";
    }

    public String scenario11() throws ExecutionException, InterruptedException {
        logger.info("Scenario 11");
        HttpRequest request = HttpRequest.newBuilder(url.resolve("/11")).build();

        // Create an inner race with 2 requests
        var innerRace = getPromises.andThen(process).apply(2, client, request);

        // Combine the inner race with another request
        var outerRace = List.of(
            CompletableFuture.supplyAsync(() -> innerRace),
            asyncCall.apply(client, request)
        );

        return process.apply(outerRace);
    }

    List<String> results() throws ExecutionException, InterruptedException, IOException {
        return List.of(scenario1(), scenario2(), scenario3(), scenario4(), scenario5(), scenario6(), scenario7(), scenario8(), scenario9(), scenario10(), scenario11());
    }
}
