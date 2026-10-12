/*
 * Copyright (c) 2026, Antonio Gabriel Muñoz Conejo <me AT tonivade DOT es>
 * Distributed under the terms of the MIT License
 */
package es.tonivade.racer;

import java.net.URI;
import java.net.http.HttpResponse;
import java.util.concurrent.CompletableFuture;

import com.github.tonivade.diesel.Diesel;

@Diesel
public interface Http {

  <T> CompletableFuture<HttpResponse<T>> get(URI request, HttpResponse.BodyHandler<T> handler);

}
