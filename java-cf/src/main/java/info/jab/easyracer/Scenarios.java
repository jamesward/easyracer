package info.jab.easyracer;

import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.logging.Logger;
import java.io.IOException;
import java.util.concurrent.TimeUnit;

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
        logger.info("scenario5");
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

    List<String> results() throws ExecutionException, InterruptedException, IOException {
        return List.of(scenario1(), scenario2(), scenario3(), scenario4(), scenario5(), scenario6(), scenario7());
    }
}
