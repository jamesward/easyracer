global.XMLHttpRequest = require('xhr2');
const { Elm } = require("../app/EasyRacer");
const { GenericContainer, AlwaysPullPolicy } = require("testcontainers");

describe("EasyRacer", () => {
  let container;
  let easyRacer;

  beforeAll(async () => {
    container = await new GenericContainer("ghcr.io/jackgene/easyracer")
      .withExposedPorts(8080)
      .withPullPolicy(new AlwaysPullPolicy())
      .start();

    easyRacer = Elm.EasyRacer.init({
      flags: {
        host: container.getHost(),
        portNumber: container.getMappedPort(8080)
      }
    });
    easyRacer.runScenario = function(scenarioNumber) {
      easyRacer.ports.nextScenario.send(scenarioNumber);
      return new Promise((resolve) => {
        let handle = function(scenarioResult) {
          resolve(scenarioResult);
          easyRacer.ports.sendResult.unsubscribe(handle);
        }
        easyRacer.ports.sendResult.subscribe(handle);
      });
    };
  }, 300_000);

  afterAll(async () => {
    await container.stop();
  });

  it("scenario 0", async () => {
    expect(await easyRacer.runScenario(0)).toBe("");
  });
});
