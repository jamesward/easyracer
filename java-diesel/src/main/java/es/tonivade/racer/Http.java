/*
 * Copyright (c) 2026, Antonio Gabriel Muñoz Conejo <me AT tonivade DOT es>
 * Distributed under the terms of the MIT License
 */
package es.tonivade.racer;

import java.net.URI;
import java.net.http.HttpResponse.BodyHandler;
import java.util.concurrent.CompletableFuture;

import com.github.tonivade.diesel.Diesel;
import com.github.tonivade.diesel.Result;

@Diesel
public interface Http {

  CompletableFuture<Result<Throwable, String>> get(URI request);

  <T> CompletableFuture<Result<Throwable, T>> get(URI request, BodyHandler<T> handler);

}
