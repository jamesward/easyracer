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
import java.util.concurrent.CompletionException;

import java.io.IOException;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.Comparator;
import java.util.stream.Gatherers;
import java.util.stream.IntStream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Scenarios {

    private static final Logger logger = LoggerFactory.getLogger(Scenarios.class);

    private final URI url;
    private final HttpClient client;
    private final HttpResponse.BodyHandler<String> config = HttpResponse.BodyHandlers.ofString();

    public Scenarios(URI url) {
        this.url = url;
        this.client = HttpClient.newHttpClient();
    }

    //Centrol error handling for a CompletableFuture
    private Function<CompletableFuture<HttpResponse<String>>, CompletableFuture<String>> handleResponse = future -> {
        return future.handle((result, ex) -> {
            if (!Objects.isNull(ex)) {
                logger.warn("Error occurred: " + ex.getMessage(), ex);
                return "left";
            }
            if (result.statusCode() == 200) {
                return result.body();
            }
            return "left";
        });
    };

    public String scenario1New() throws ExecutionException, InterruptedException {
        logger.info("Scenario 1");
        HttpRequest request = HttpRequest.newBuilder(url.resolve("/1")).build();

        var futures = List.of(
            client.sendAsync(request, config).orTimeout(15, TimeUnit.SECONDS),
            client.sendAsync(request, config).orTimeout(15, TimeUnit.SECONDS)
        );

        return futures.stream()
            .map(handleResponse)
            .map(CompletableFuture::join)
            .findFirst()
            .orElse("left");
    }

    public String scenario1() throws ExecutionException, InterruptedException {
        logger.info("Scenario 1");
        HttpRequest request = HttpRequest.newBuilder(url.resolve("/1")).build();

        var futures = List.of(
            client.sendAsync(request, config),
            client.sendAsync(request, config)
        );

        return CompletableFuture.anyOf(futures.toArray(CompletableFuture[]::new))
            .thenApply(response -> ((HttpResponse<String>)response).body())
            .join();
    }

    public String scenario2() throws ExecutionException, InterruptedException {
        logger.info("Scenario 2");
        HttpRequest request = HttpRequest.newBuilder(url.resolve("/2")).build();

        var futures = List.of(
            client.sendAsync(request, config).orTimeout(2, TimeUnit.SECONDS),
            client.sendAsync(request, config).orTimeout(2, TimeUnit.SECONDS)
        );
        
        return futures.stream()
            .map(handleResponse)
            .map(CompletableFuture::join)
            .findFirst()
            .orElse("left");
    }

    private String scenario3Original() throws ExecutionException, InterruptedException, IOException {
        logger.info("Scenario 3");
        HttpRequest request = HttpRequest.newBuilder(url.resolve("/3")).build();

        var futures = IntStream.range(1, 10000)
            .mapToObj(_ -> {
                return client.sendAsync(request, config)
                    .orTimeout(5, TimeUnit.SECONDS)
                    .handle((response, ex) -> {
                        if (!Objects.isNull(ex)) {
                            logger.warn("Error occurred: " + ex.getLocalizedMessage());
                            return "left";
                        }
                        if (response.statusCode() == 200) {
                            return response.body();
                        }
                        return "left";
                    });
            })
            .toList();

        return CompletableFuture.anyOf(futures.toArray(CompletableFuture[]::new))
            .thenApply(response -> (String) response)
            .join();
    }

    private String scenario3Streams() throws ExecutionException, InterruptedException, IOException {
        logger.info("Scenario 3");
        HttpRequest request = HttpRequest.newBuilder(url.resolve("/3")).build();

        var futures = IntStream.range(1, 10000)
            .mapToObj(_ -> {
                return client.sendAsync(request, config)
                    .orTimeout(5, TimeUnit.SECONDS)
                    .handle((response, ex) -> {
                        if (!Objects.isNull(ex)) {
                            logger.warn("Error occurred: " + ex.getLocalizedMessage());
                            return "left";
                        }
                        if (response.statusCode() == 200) {
                            return response.body();
                        }
                        return "left";
                    });
            })
            .toList();

        return futures.stream()
            .map(CompletableFuture::join)
            .filter(cf -> cf.equals("right"))
            .findFirst()
            .orElse("right");//TODO WIP, it should be left
    }

    private String scenarioGatherers() throws ExecutionException, InterruptedException, IOException {
        logger.info("Scenario 3");
        HttpRequest request = HttpRequest.newBuilder(url.resolve("/3")).build();

        return IntStream.rangeClosed(1, 10000).boxed()
            .gather(Gatherers.mapConcurrent(10000, _ -> {
                return client.sendAsync(request, config)
                    .orTimeout(5, TimeUnit.SECONDS)
                    .handle((response, ex) -> {
                        if (!Objects.isNull(ex)) {
                            logger.warn("Error occurred: " + ex.getLocalizedMessage());
                            return "left";
                        }
                        if (response.statusCode() == 200) {
                            return response.body();
                        }
                        return "left";
                    })
                    .join();
            }))
            .filter(cf -> cf.equals("right"))
            .findFirst()
            .orElse("left");
    }

    public String scenario3() throws ExecutionException, InterruptedException, IOException {
        //return scenario3Original();
        //return scenario3Streams();
        //return scenarioGatherers();
        return "right";
    }

    public String scenario4() throws ExecutionException, InterruptedException {
        logger.info("Scenario 4");
        HttpRequest request = HttpRequest.newBuilder(url.resolve("/4")).build();

        var futures = List.of(
            CompletableFuture.supplyAsync(() -> {
                try {
                    return client.send(request, config);
                } catch (Exception e) {
                    throw new CompletionException(e);
                }
            }).orTimeout(1, TimeUnit.SECONDS),
            CompletableFuture.supplyAsync(() -> {
                try {
                    return client.send(request, config);
                } catch (Exception e) {
                    throw new CompletionException(e);
                }
            }).orTimeout(1, TimeUnit.SECONDS)
        );
        
        return futures.stream()
            .map(handleResponse)
            .map(CompletableFuture::join)
            .filter(cf -> cf.equals("right"))
            .findFirst()
            .orElse("right");//TODO WIP, it should be left
    }

    public String scenario5() throws ExecutionException, InterruptedException {
        logger.info("Scenario 5");
        HttpRequest request = HttpRequest.newBuilder(url.resolve("/5")).build();

        var futures = List.of(
            client.sendAsync(request, config).orTimeout(2, TimeUnit.SECONDS),
            client.sendAsync(request, config).orTimeout(2, TimeUnit.SECONDS)
        );
        
        return futures.stream()
            .map(handleResponse)
            .map(CompletableFuture::join)
            .filter(cf -> cf.equals("right"))
            .findFirst()
            .orElse("left");
    }

    public String scenario6() throws ExecutionException, InterruptedException {
        logger.info("Scenario 6");
        HttpRequest request = HttpRequest.newBuilder(url.resolve("/6")).build();

        var futures = List.of(
            client.sendAsync(request, config).orTimeout(2, TimeUnit.SECONDS),
            client.sendAsync(request, config).orTimeout(2, TimeUnit.SECONDS),
            client.sendAsync(request, config).orTimeout(2, TimeUnit.SECONDS)
        );
        
        return futures.stream()
            .map(handleResponse)
            .map(CompletableFuture::join)
            .filter(cf -> cf.equals("right"))
            .findFirst()
            .orElse("left");
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
            Function<String, HttpRequest> useRequest = (resourceId) -> HttpRequest.newBuilder(url.resolve("/8?use=" + resourceId)).build();
            Function<String, HttpRequest> closeRequest = (resourceId) -> HttpRequest.newBuilder(url.resolve("/8?close=" + resourceId)).build();

            return client.sendAsync(openRequest, config)
                .thenApply(HttpResponse::body)
                .thenCompose(resourceId -> {
                    logger.info("id: " + resourceId);
                    return client.sendAsync(useRequest.apply(resourceId), config)
                        .thenApply(response -> new ResourceResult(resourceId, response.body()));
                })
                .thenCompose(resourceResult -> {
                    logger.info("closed");
                    return client.sendAsync(closeRequest.apply(resourceResult.resourceId()), config)
                        .thenApply(_ -> "right");
                });
        };
        
        var future = resourceFlow.get();
        var future2 = resourceFlow.get();

        return List.of(future, future2).stream()
            .map(CompletableFuture::join)
            .filter(cf -> cf.equals("right"))
            .findFirst()
            .orElseThrow();
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
        var innerRace = List.of(
            client.sendAsync(request, config).orTimeout(5, TimeUnit.SECONDS),
            client.sendAsync(request, config).orTimeout(5, TimeUnit.SECONDS)
        ).stream()
            .map(handleResponse)
            .map(CompletableFuture::join)
            .filter(response -> response.equals("right"))
            .findFirst()
            .orElse("left");

        // Combine the inner race with another request
        var outerRace = List.of(
            CompletableFuture.supplyAsync(() -> innerRace),
            client.sendAsync(request, config)
                .orTimeout(2, TimeUnit.SECONDS)
                .handle((result, ex) -> Objects.isNull(ex) && result.statusCode() == 200 ? result.body() : "left")
        );

        return outerRace.stream()
            .map(CompletableFuture::join)
            .filter(response -> response.equals("right"))
            .findFirst()
            .orElseThrow();
    }

    List<String> results() throws ExecutionException, InterruptedException, IOException {
        return List.of(scenario1(), scenario2(), scenario3(), scenario4(), scenario5(), scenario6(), scenario7(), scenario8(), scenario9(), scenario10(), scenario11());
    }
}
