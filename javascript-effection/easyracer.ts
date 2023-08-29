import {
  action,
  call,
  createContext,
  expect as $await,
  type Operation,
  sleep,
  spawn,
  type Task,
  useAbortSignal,
} from "effection";

export const BaseURL = createContext<string>(
  "BaseURL",
  "http://localhost:8080",
);
export const Scenario = createContext<string>("Scenario");

export function* delay<T>(millis: number, op: Operation<T>): Operation<T> {
  yield* sleep(millis);
  return yield* op;
}

/**
 * returns either the result of `op`, or a timeout string after `limit` ms
 */
export function timeout<T>(
  limit: number,
  op: Operation<T>,
): Operation<T | string> {
  return action(function* (resolve) {
    yield* spawn(function* () {
      yield* sleep(limit);
      resolve(`timeout of ${limit}ms exceeded`);
    });

    resolve(yield* op);
  });
}

/**
 * Execute a request corresponding to the current scenario.
 */
export const request = (query?: string) =>
  call(function* () {
    let signal = yield* useAbortSignal();
    let url = `${yield* BaseURL}/${yield* Scenario}${query ? "?" + query : ""}`;

    let request = fetch(url, { signal });

    try {
      let response = yield* $await(request);
      return yield* $await(response.text());
    } catch (error) {
      return String(error);
    }
  });

/**
 * return the result of the first operation whose response is "right". Or the
 * last result that wasn't "right";
 */
export function rightOrNot(ops: Operation<string>[]): Operation<string> {
  return action(function* (resolve) {
    let tasks: Task<string>[] = [];
    for (let operation of ops) {
      let task = yield* spawn(function* () {
        let result = yield* operation;
        if (result === "right") {
          // right!
          resolve("right");
        }
        return result;
      });
      tasks.push(task);
    }
    let last: string | null = null;
    for (let task of tasks) {
      let result = yield* task;
      if (result !== "right") {
        last = result;
      }
    }
    // not!
    resolve(String(last));
  });
}
