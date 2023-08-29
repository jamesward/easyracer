import {
  afterEach,
  beforeEach,
  describe as $describe,
  it,
} from "node:test";
import { expect } from "expect";
import {
  action,
  createScope,
  type Operation,
  spawn,
  type Task,
} from "effection";

import { Scenario } from "./easyracer.ts";

let [scope, destroy] = createScope();

export function describe(name: string, fn: () => void): void {
  $describe(name, () => {
    beforeEach(() => {
      [scope, destroy] = createScope();
    });

    afterEach(destroy);

    fn();
  });
}

type ScenarioFn = () => Operation<unknown> | Operation<unknown>[];

export function scenario(number: string, fn: ScenarioFn): void {
  it(`scenario ${number}`, runScenario(number, fn));
}

scenario.only = (number: string, fn: ScenarioFn) => {
  it.only(`scenario ${number}`, runScenario(number, fn));
};

scenario.skip = (number: string, _op: ScenarioFn) => {
  it.skip(`scenario ${number}`, () => {});
};

export function rightOrNot(ops: Operation<unknown>[]): Operation<string> {
  return action(function* (resolve) {
    let tasks: Task<unknown>[] = [];
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
    let last: unknown = null;
    for (let task of tasks) {
      last = yield* task;
    }
    // not!
    resolve(String(last));
  });
}

function runScenario(number: string, fn: ScenarioFn) {
  return () =>
    scope.run(function* () {
      yield* Scenario.set(number);
      let scenario = fn();
      let ops = Array.isArray(scenario) ? scenario : [scenario];

      let result = yield* rightOrNot(ops);

      expect(result).toEqual("right");
    });
}
