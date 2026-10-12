/*
 * Copyright (c) 2026, Antonio Gabriel Muñoz Conejo <me AT tonivade DOT es>
 * Distributed under the terms of the MIT License
 */
package es.tonivade.racer;

import static java.net.http.HttpClient.Redirect.NORMAL;
import static java.net.http.HttpClient.Version.HTTP_1_1;

import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.time.Duration;
import java.util.concurrent.CompletableFuture;

public class HttpImpl implements Http {

  private final HttpClient client = HttpClient.newBuilder()
    .version(HTTP_1_1)
    .followRedirects(NORMAL)
    .connectTimeout(Duration.ofSeconds(5))
    .build();

  @Override
  public <T> CompletableFuture<HttpResponse<T>> get(URI uri, HttpResponse.BodyHandler<T> handler) {
    var request = HttpRequest.newBuilder().GET().uri(uri).timeout(Duration.ofSeconds(5)).build();
    return client.sendAsync(request, handler);
  }
}
