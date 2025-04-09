import { type Operation, sleep } from "effection";
import os from "node:os";

export function* loadAvg(): Operation<number> {
  let start = getCPUInfo();

  yield* sleep(1000);

  let end = getCPUInfo();
  let idle = end.idle - start.idle;
  let total = end.total - start.total;
  return idle / total;
}

function getCPUInfo() {
  let cpus = os.cpus();

  let user = 0;
  let nice = 0;
  let sys = 0;
  let idle = 0;
  let irq = 0;

  for (let cpu in cpus) {
    if (!Object.hasOwn(cpus, cpu)) continue;
    user += cpus[cpu].times.user;
    nice += cpus[cpu].times.nice;
    sys += cpus[cpu].times.sys;
    irq += cpus[cpu].times.irq;
    idle += cpus[cpu].times.idle;
  }

  let total = user + nice + sys + idle + irq;

  return {
    "idle": idle,
    "total": total,
  };
}
