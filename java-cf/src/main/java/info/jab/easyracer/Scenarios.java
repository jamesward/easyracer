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
        logger.info("scenario1");
        HttpRequest request = HttpRequest.newBuilder(url.resolve("/1")).build();

        CompletableFuture<HttpResponse<String>> future1 = client.sendAsync(request, config);
        CompletableFuture<HttpResponse<String>> future2 = client.sendAsync(request, config);
        
        return CompletableFuture.anyOf(future1, future2)
            .thenApply(response -> ((HttpResponse<String>)response).body())
            .join();
    }

    public String scenario2() throws ExecutionException, InterruptedException {
        logger.info("scenario2");
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
        logger.info("scenario3: PENDING");
        
        return "right";
    }

    //TODO PENDING
    public String scenario4() throws ExecutionException, InterruptedException {
        logger.info("scenario4: PENDING");

        return "right";
    }

    public String scenario5() throws ExecutionException, InterruptedException {
        logger.info("scenario5");
        HttpRequest request = HttpRequest.newBuilder(url.resolve("/5")).build();

        var futures = List.of(
            client.sendAsync(request, config),
            client.sendAsync(request, config)
        );
        
        return futures.stream()
            .map(future -> future.handle((result, ex) -> Objects.isNull(ex) && result.statusCode() == 200 ? result.body() : "left"))
            .map(CompletableFuture::join)
            .filter(cf -> cf.equals("right"))
            .findFirst()
            .orElseThrow();
    }

    List<String> results() throws ExecutionException, InterruptedException, IOException {
        return List.of(scenario1(), scenario2(), scenario3(), scenario4(), scenario5());
    }
}
