package info.jab.easyracer;

import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.net.http.HttpResponse.BodyHandlers;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

public class Scenarios {

    private final URI url;
    private final HttpClient client;

    public Scenarios(URI url) {
        this.url = url;
        this.client = HttpClient.newHttpClient();
    }

    public String scenario1() throws ExecutionException, InterruptedException {
        HttpRequest request = HttpRequest.newBuilder(url.resolve("/1")).build();
        var config = HttpResponse.BodyHandlers.ofString();
        CompletableFuture<HttpResponse<String>> future1 = client.sendAsync(request, config);
        CompletableFuture<HttpResponse<String>> future2 = client.sendAsync(request, config);
        
        return CompletableFuture.anyOf(future1, future2)
            .thenApply(response -> ((HttpResponse<String>)response).body())
            .join();
    }

    public String scenario2() throws ExecutionException, InterruptedException {
        HttpRequest request = HttpRequest.newBuilder(url.resolve("/2")).build();
        var config = HttpResponse.BodyHandlers.ofString();
        var CONNECTION_ERROR = "Connection error";

        var futures = List.of(
            client.sendAsync(request, config).thenApply(HttpResponse::body),
            client.sendAsync(request, config).thenApply(HttpResponse::body)
        );
        
        return futures.stream()
            .map(cf -> cf.exceptionally(throwable -> CONNECTION_ERROR)) // Java doesn´t java Either natively
            .map(CompletableFuture::join)
            .filter(response -> !response.equals(CONNECTION_ERROR))
            .findFirst()
            .orElseThrow();
    }

    List<String> results() throws ExecutionException, InterruptedException {
            return List.of(scenario1(), scenario2());
    }
}
