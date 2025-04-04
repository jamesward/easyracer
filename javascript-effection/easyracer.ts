import { call, type Operation, race, scoped, sleep } from "effection";

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
  return race([
    op,
    call(function* () {
      yield* sleep(limit);
      return `timeout of ${limit}ms exceeded`;
    }),
  ]);
}

export function createRequestFn(base: string, scenario: string) {
  return (query?: string) =>
    scoped<string>(function* () {
      let controller = new AbortController();
      let { signal } = controller;

      let url = `${base}/${scenario}${query ? "?" + query : ""}`;

      let promises: Promise<unknown>[] = [];

      let request = fetch(url, { signal });

      promises.push(request);

      try {
        let response = yield* call(() => request);
        let text = response.text();
        promises.push(text);

        return yield* call(() => text);
      } catch (error) {
        return String(error);
      } finally {
        controller.abort();
        yield* call(() => Promise.allSettled(promises));
      }
    });
}
