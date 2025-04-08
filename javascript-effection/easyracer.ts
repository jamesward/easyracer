import {
  call,
  type Operation,
  race,
  scoped,
  sleep,
  spawn,
  until,
  withResolvers,
} from "effection";

/**
 * Search for a operation that results in a TestResponse whose
 * body is "right"
 */
export const winner = find((response: TestResponse) =>
  response.body === "right"
);

export const first = find(() => true);

/**
 * Search for operation results in parallel.
 *
 * `find()` takes `predicate` and then returns an operation
 * that will run a list of operations in parallel that returns
 * the first matching result or a "not found" value if no
 * result matches `predicate`.
 */
export function find<T>(
  predicate: (value: T) => boolean,
): (ops: Operation<T>[]) => Operation<T | NotFound<T>> {
  return function* (ops) {
    let winner = withResolvers<T | NotFound<T>>();
    let losers: T[] = [];

    for (let operation of ops) {
      yield* spawn(function* () {
        let result = yield* operation;
        if (predicate(result)) {
          winner.resolve(result);
        } else {
          losers.push(result);
          if (losers.length === ops.length) {
            winner.resolve({
              status: 404,
              body: "Not Found",
              misses: losers,
            });
          }
        }
      });
    }

    return yield* winner.operation;
  };
}

/**
 * A test response that is returned when no matching
 * response is found.
 */
export interface NotFound<T> extends TestResponse {
  status: 404;
  body: "Not Found";
  misses: T[];
}

/**
 * Wait `millis` milliseconds before evaluating `op`
 */
export function* delay<T>(millis: number, op: Operation<T>): Operation<T> {
  yield* sleep(millis);
  return yield* op;
}

/**
 * A Value representing an operation that timed out.
 */
export interface TimeoutResponse {
  status: 408;
  body: "Request Timeout";
}

/**
 * Runs `operation`, but if it does not complete within `limit` milliseconds, will return
 * a `TimeoutResponse` instead.
 */
export function timeout<T>(
  limit: number,
  operation: Operation<T>,
): Operation<T | TimeoutResponse> {
  return race([
    operation,
    call(function* () {
      yield* sleep(limit);
      return { status: 408, body: "Request Timeout" } as const;
    }),
  ]);
}

/**
 * The shape of all the easyracer server responses.
 */
export interface TestResponse {
  status: number;
  body: string;
}

export type TestRequest = Operation<TestResponse>;

export function createRequestFn(base: string, scenario: number) {
  return (query?: string): TestRequest =>
    scoped(function* () {
      let controller = new AbortController();
      let { signal } = controller;

      let url = `${base}/${scenario}${query ? "?" + query : ""}`;

      let request = fetch(url, { signal });

      try {
        let response = yield* until(request);
        let text = response.text();

        return {
          status: response.status,
          body: yield* until(text),
        };
      } catch (error) {
        return { status: 400, body: String(error) };
      } finally {
        controller.abort();
        let [result] = yield* until(Promise.allSettled([request]));
        if (result.status === "fulfilled") {
          yield* until(Promise.allSettled([result.value.bytes()]));
        }
      }
    });
}
