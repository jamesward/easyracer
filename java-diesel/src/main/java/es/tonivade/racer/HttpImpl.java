/*
 * Copyright (c) 2026, Antonio Gabriel Muñoz Conejo <me AT tonivade DOT es>
 * Distributed under the terms of the MIT License
 */
package es.tonivade.racer;

import static com.github.tonivade.diesel.Result.failure;
import static com.github.tonivade.diesel.Result.success;
import static java.net.http.HttpClient.Redirect.NORMAL;
import static java.net.http.HttpClient.Version.HTTP_1_1;

import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.net.http.HttpResponse.BodyHandler;
import java.time.Duration;
import java.util.concurrent.CompletableFuture;

import com.github.tonivade.diesel.Result;

public class HttpImpl implements Http {

  private final HttpClient client = HttpClient.newBuilder()
    .version(HTTP_1_1)
    .followRedirects(NORMAL)
    .connectTimeout(Duration.ofSeconds(5))
    .build();

  @Override
  public CompletableFuture<Result<Throwable, String>> get(URI request) {
    return get(request, HttpResponse.BodyHandlers.ofString());
  }

  @Override
  public <T> CompletableFuture<Result<Throwable, T>> get(URI uri, BodyHandler<T> handler) {
    var request = HttpRequest.newBuilder().GET().uri(uri).timeout(Duration.ofSeconds(5)).build();

    return client.sendAsync(request, handler)
        .<Result<Throwable, T>>thenApply(response -> {
          if (response.statusCode() == 200) {
            return success(response.body());
          }
          return failure(new RuntimeException("error: " + response.statusCode()));
        })
        .exceptionally(Result::failure);
  }
}
