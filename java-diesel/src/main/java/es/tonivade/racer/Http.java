/*
 * Copyright (c) 2026, Antonio Gabriel Muñoz Conejo <me AT tonivade DOT es>
 * Distributed under the terms of the MIT License
 */
package es.tonivade.racer;

import java.net.URI;
import java.net.http.HttpResponse.BodyHandler;

import com.github.tonivade.diesel.Diesel;
import com.github.tonivade.diesel.Result;

@Diesel(errorType = Throwable.class)
public interface Http {

  Result<Throwable, String> get(URI request);

  <T> Result<Throwable, T> get(URI request, BodyHandler<T> handler);

}
