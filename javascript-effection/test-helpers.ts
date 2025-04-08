import { afterEach, beforeEach, describe as $describe, it } from "node:test";
import { createScope, type Operation } from "effection";
import { createRequestFn, type TestRequest } from "./easyracer.ts";

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

type ScenarioFn = (
  request: (
    query?: string,
  ) => TestRequest,
) => Operation<void>;

export function scenario(number: number, fn: ScenarioFn): void {
  it(`scenario ${number}`, runScenario(number, fn));
}

scenario.only = (number: number, fn: ScenarioFn) => {
  it.only(`scenario ${number}`, runScenario(number, fn));
};

scenario.skip = (number: string, _op: ScenarioFn) => {
  it.skip(`scenario ${number}`, () => {});
};

function runScenario(number: number, fn: ScenarioFn) {
  return () =>
    scope.run(function* () {
      let base = `http://localhost:8080`;
      let scenario = fn(createRequestFn(base, number));
      yield* scenario;
    });
}
