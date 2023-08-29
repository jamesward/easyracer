import { describe, scenario } from "./test-helpers.ts";
import { spawn } from "effection";
import { delay, request, timeout } from "./easyracer.ts";

describe("easyracer", () => {
  scenario("1", () => [request(), request()]);
  scenario("2", () => [request(), request()]);
  scenario.skip("3", () => Array(10_000).fill(request()));
  scenario("4", () => [request(), timeout(1000, request())]);
  scenario("5", () => [request(), request()]);
  scenario("6", () => [request(), request(), request()]);
  scenario("7", () => [request(), delay(3000, request())]);
  scenario("8", () => {
    function* res() {
      let id = yield* request("open");
      try {
        return yield* request(`use=${id}`);
      } finally {
        yield* request(`close=${id}`);
      }
    }

    return [res(), res()];
  });

  scenario("9", function* () {
    let answer = "";
    let tasks = [];
    for (let i = 0; i < 10; i++) {
      let task = yield* spawn(function* () {
        let result = yield* request();
        if (result.length === 1) {
          answer += result;
        }
      });
      tasks.push(task);
    }

    for (let task of tasks) {
      yield* task;
    }

    return answer;
  });
});
