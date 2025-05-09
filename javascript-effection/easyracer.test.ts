import { describe, scenario } from "./test-helpers.ts";
import { spawn, until, withResolvers } from "effection";
import { delay, first, timeout, winner } from "./easyracer.ts";
import { expect } from "expect";
import { loadAvg } from "./cpu.ts";

describe("easyracer", () => {
  scenario(1, function* (fetch) {
    let result = yield* winner([fetch(), fetch()]);
    expect(result).toEqual({ status: 200, body: "right" });
  });
  scenario(2, function* (fetch) {
    let result = yield* winner([fetch(), fetch()]);
    expect(result).toEqual({ status: 200, body: "right" });
  });
  scenario(3, function* (fetch) {
    let requests = Array(10_000).fill(fetch());
    let result = yield* winner(requests);
    expect(result).toEqual({ status: 200, body: "right" });
  });

  scenario(4, function* (fetch) {
    let result = yield* winner([fetch(), timeout(1000, fetch())]);
    expect(result).toEqual({ status: 200, body: "right" });
  });

  scenario(5, function* (fetch) {
    let result = yield* winner([fetch(), fetch()]);
    expect(result).toEqual({ status: 200, body: "right" });
  });
  scenario(6, function* (fetch) {
    let result = yield* winner([fetch(), fetch(), fetch()]);
    expect(result).toEqual({ status: 200, body: "right" });
  });
  scenario(7, function* (fetch) {
    let result = yield* winner([fetch(), delay(3000, fetch())]);
    expect(result).toEqual({ status: 200, body: "right" });
  });
  scenario(8, function* (fetch) {
    function* resource() {
      let { body: id } = yield* fetch("open");
      try {
        return yield* fetch(`use=${id}`);
      } finally {
        yield* fetch(`close=${id}`);
      }
    }

    let result = yield* winner([resource(), resource()]);
    expect(result).toEqual({ status: 200, body: "right" });
  });

  scenario(9, function* (fetch) {
    let requests = Array<ReturnType<typeof fetch>>(10).fill(fetch());
    let letters: string[] = [];
    let answer = withResolvers<string>();
    for (let request of requests) {
      yield* spawn(function* () {
        let result = yield* request;
        if (result.status === 200) {
          letters.push(result.body);
          if (letters.length === "right".length) {
            answer.resolve(letters.join(""));
          }
        }
      });
    }

    let result = yield* answer.operation;
    expect(result).toEqual("right");
  });

  scenario(10, function* (fetch) {
    function* computeSha() {
      let encoder = new TextEncoder();
      while (true) {
        yield* until(
          crypto.subtle.digest("SHA-256", encoder.encode("Easy Racer")),
        );
      }
    }

    function* reportLoad() {
      while (true) {
        let load = yield* loadAvg();
        yield* fetch(`load=${load}`);
      }
    }

    let result = yield* first([fetch("load"), computeSha(), reportLoad()]);

    expect(result).toMatchObject({ status: 200 });
  });

  scenario(11, function* (fetch) {
    let result = yield* winner([fetch(), winner([fetch(), fetch()])]);
    expect(result).toEqual({ status: 200, body: "right" });
  });
});
