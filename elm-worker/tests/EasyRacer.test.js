global.XMLHttpRequest = require('xhr2');
const { GenericContainer, Wait } = require("testcontainers");
const os = require("os-utils");

describe("EasyRacer", () => {
  let container;

  beforeAll(async () => {
    container = await new GenericContainer("ghcr.io/jamesward/easyracer")
      .withExposedPorts(8080)
      .withWaitStrategy(Wait.forHttp("/", 8080))
      .start();
  }, 30_000);

  afterAll(async () => {
    await container.stop();
  });

  for (const idx of Array(10).keys()) {
    const scenarioNum = idx + 1;
    it("scenario " + scenarioNum, async () => {
      const name = "Scenario" + scenarioNum;
      const { Elm } = require("../app/EasyRacer/" + name);
      const scenario = Elm.EasyRacer[name].init({
        flags: `http://${container.getHost()}:${container.getMappedPort(8080)}`
      });
      // For scenario 10
      if (typeof scenario.ports.sendCpuLoadRequest !== "undefined") {
        scenario.ports.sendCpuLoadRequest.subscribe(function () {
          os.cpuUsage(function (cpuLoad) {
            os.countCPUs(function (cpuCount) {
              console.log("CPU: " + cpuLoad * 100 + "% x " + cpuCount);
              scenario.ports.receiveCpuLoadResponse.send(cpuLoad * cpuCount);
            });
          });
        });
      }
      if (typeof scenario.ports.sendFetchRequest !== "undefined") {
        scenario.ports.sendFetchRequest.subscribe(function (url) {
          fetch(url, {"redirect": "manual"}).then(response => {
            response.text().then(text => {
              scenario.ports.receiveFetchResponse.send({
                "statusCode": response.status,
                "bodyText": text
              });
            });
          });
        });
      }

      let resultPromise = new Promise((resolve, reject) => {
        scenario.ports.sendResult_.subscribe(function(scenarioResult) {
          if (scenarioResult.isError) {
            reject(new Error(scenarioResult.value));
          } else {
            resolve(scenarioResult.value);
          }
        });
      });

      expect(await resultPromise).toBe("right");
    }, 30_000);
  }
});
