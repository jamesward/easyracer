global.XMLHttpRequest = require('xhr2');
const { Elm } = require("../app/EasyRacer");
const { GenericContainer, AlwaysPullPolicy, Wait } = require("testcontainers");

describe("EasyRacer", () => {
  let container;
  let easyRacer;

  beforeAll(async () => {
    container = await new GenericContainer("ghcr.io/jackgene/easyracer")
      .withExposedPorts(8080)
      .withPullPolicy(new AlwaysPullPolicy())
      .withWaitStrategy(Wait.forHttp("/", 8080))
      .start();

    easyRacer = Elm.EasyRacer.init({
      flags: {
        host: container.getHost(),
        portNumber: container.getMappedPort(8080)
      }
    });
    easyRacer.resultPromiseCompletions = [];
    easyRacer.ports.sendResult.subscribe(function(scenarioResult) {
      const completion = easyRacer.resultPromiseCompletions.shift();
      if (scenarioResult.isError) {
        completion.reject(new Error(scenarioResult.value));
      } else {
        completion.resolve(scenarioResult.value);
      }
    });
    easyRacer.runScenario = function(scenarioNumber) {
      easyRacer.ports.nextScenario.send(scenarioNumber);
      return new Promise((resolve, reject) => {
        easyRacer.resultPromiseCompletions.push({
          resolve: resolve,
          reject: reject
        });
      });
    };
  });

  afterAll(async () => {
    await container.stop();
  });

  for (const idx of Array(9).keys()) {
    const scenarioNum = idx + 1
    it("scenario " + scenarioNum, async () => {
      expect(await easyRacer.runScenario(scenarioNum)).toBe("right");
    });
  }
});
