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
import java.util.logging.Logger;
import java.io.IOException;
import java.util.concurrent.TimeUnit;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.Comparator;
import java.util.stream.IntStream;
import java.security.MessageDigest;

public class Scenarios {

    private static final Logger logger = Logger.getLogger(Scenarios.class.getName());

    private final URI url;
    private final HttpClient client;
    private final HttpResponse.BodyHandler<String> config = HttpResponse.BodyHandlers.ofString();

    public Scenarios(URI url) {
        this.url = url;
        this.client = HttpClient.newHttpClient();
    }

    public String scenario1() throws ExecutionException, InterruptedException {
        logger.info("Scenario 1");
        HttpRequest request = HttpRequest.newBuilder(url.resolve("/1")).build();

        var future1 = client.sendAsync(request, config);
        var future2 = client.sendAsync(request, config);
        
        return CompletableFuture.anyOf(
            future1.thenApply(HttpResponse::body), 
            future2.thenApply(HttpResponse::body))
        .thenApply(response -> (String) response)
        .join();
    }

    public String scenario2() throws ExecutionException, InterruptedException {
        logger.info("Scenario 2");
        HttpRequest request = HttpRequest.newBuilder(url.resolve("/2")).build();

        var futures = List.of(
            client.sendAsync(request, config).thenApply(HttpResponse::body),
            client.sendAsync(request, config).thenApply(HttpResponse::body)
        );
        
        return futures.stream()
            .filter(cf -> !cf.isCompletedExceptionally())
            .map(CompletableFuture::join)
            .findFirst()
            .orElseThrow();
    }

    //TODO PENDING
    public String scenario3() throws ExecutionException, InterruptedException, IOException {
        logger.info("Scenario 3: PENDING");
        
        return "right";
    }

    //TODO PENDING
    public String scenario4() throws ExecutionException, InterruptedException {
        logger.info("Scenario 4: PENDING");

        return "right";
    }

    public String scenario5() throws ExecutionException, InterruptedException {
        logger.info("Scenario 5");
        HttpRequest request = HttpRequest.newBuilder(url.resolve("/5")).build();

        var futures = List.of(
            client.sendAsync(request, config).orTimeout(2, TimeUnit.SECONDS),
            client.sendAsync(request, config).orTimeout(2, TimeUnit.SECONDS)
        );
        
        return futures.stream()
            .map(future -> future.handle((result, ex) -> Objects.isNull(ex) && result.statusCode() == 200 ? result.body() : "left"))
            .map(CompletableFuture::join)
            .filter(cf -> cf.equals("right"))
            .findFirst()
            .orElseThrow();
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
            .map(future -> future.handle((result, ex) -> Objects.isNull(ex) && result.statusCode() == 200 ? result.body() : "left"))
            .map(CompletableFuture::join)
            .filter(cf -> cf.equals("right"))
            .findFirst()
            .orElseThrow();
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

    static class Req implements AutoCloseable {

        private final HttpClient client;
        private final URI url;

        final Function<URI, HttpRequest> openReq = (url) ->
                HttpRequest.newBuilder(url.resolve("/8?open")).build();
        final BiFunction<String, URI, HttpRequest> useReq = (id, url) ->
                HttpRequest.newBuilder(url.resolve("/8?use=" + id)).build();
        final BiFunction<String, URI, HttpRequest> closeReq = (id, url) ->
                HttpRequest.newBuilder(url.resolve("/8?close=" + id)).build();

        private final HttpResponse.BodyHandler<String> config = HttpResponse.BodyHandlers.ofString();

        final String id;

        public Req(HttpClient client, URI url) throws IOException, InterruptedException {
            this.client = client;
            this.url = url;
            id = client.send(openReq.apply(url), config).body();
            logger.info("id: " + id);
        }

        HttpResponse<String> make() throws Exception {
            var resp = client.send(useReq.apply(id, url), config);
            if (resp.statusCode() == 200) {
                return resp;
            } else {
                throw new Exception("invalid response " + resp.body());
            }
        }

        @Override
        public void close() throws IOException, InterruptedException {
            client.send(closeReq.apply(id, url), config).body();
            logger.info("closed");
        }
    }

    public String scenario8() throws ExecutionException, InterruptedException {
        logger.info("Scenario 8");

        /*
        record ResourceResult(String resourceId, String result) {}

        HttpRequest openRequest = HttpRequest.newBuilder(url.resolve("/8?open")).build();
        Function<String, HttpRequest> useRequest = (resourceId) -> HttpRequest.newBuilder(url.resolve("/8?use=" + resourceId)).build();
        Function<String, HttpRequest> closeRequest = (resourceId) -> HttpRequest.newBuilder(url.resolve("/8?close=" + resourceId)).build();

        var future = client.sendAsync(openRequest, config)
            .thenApply(HttpResponse::body)
            .thenCompose(resourceId -> {
                logger.info("resourceId: " + resourceId);
                return client.sendAsync(useRequest.apply(resourceId), config)
                    .thenApply(response -> new ResourceResult(resourceId, response.body()));
            })
            .thenCompose(resourceResult -> 
                client.sendAsync(closeRequest.apply(resourceResult.resourceId()), config)
                    .thenApply(_ -> resourceResult.result())
            );
        */

        CompletableFuture<String> future1 = CompletableFuture.supplyAsync(() -> {
            try (var req = new Req(client, url)) {
                return req.make().body();
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        })
        .orTimeout(5, TimeUnit.SECONDS)
        .handle((response, ex) -> {
            if (ex != null) {
                logger.info("ex: " + ex);
                return "left";
            }
            return response;
        });

        CompletableFuture<String> future2 = CompletableFuture.supplyAsync(() -> {
            try (var req = new Req(client, url)) {
                return req.make().body();
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        })
        .orTimeout(5, TimeUnit.SECONDS)
        .handle((response, ex) -> {
            if (ex != null) {
                logger.info("ex: " + ex);
                return "left";
            }
            return response;
        });

        return List.of(future1, future2).stream()
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
            .map(future -> future.handle((result, ex) -> Objects.isNull(ex) && result.statusCode() == 200 ? result.body() : "left"))
            .map(CompletableFuture::join)
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
