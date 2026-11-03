# AGENTS.md

Easy Racer compares how languages and frameworks handle structured concurrency. Each top-level directory (e.g. `typescript-effect`, `scala-zio`) is an independent client that implements the 11 scenarios described in `README.md`. `scenario-server` is the server that validates them, published as the image `ghcr.io/jamesward/easyracer`.

Clients are tested against that image, usually through Testcontainers. CI is defined in `.github/workflows/<client>.yaml`, generated from `.github/workflows/clients.pkl`. A client directory may have its own `AGENTS.md` with client-specific instructions.

## Local environment: rootless Docker breaks scenario 3

This machine runs **rootless Docker**. Its port forwarding (RootlessKit) can't reliably handle scenario 3, which opens 10,000 concurrent connections. When a client reaches the server through a published port (`localhost:<mapped port>`, which is what Testcontainers uses), scenario 3 intermittently hangs: the expected `right` response never arrives, and sometimes hundreds of extra `ECONNRESET`s appear. The test then sits until its timeout, which looks like the test run is stuck.

This is an environment problem, not a client bug. CI runs on GitHub's regular (rootful) Docker and isn't affected.

Evidence from 2026-11-03 (typescript-effect, scenario 3 run on its own):
- Through `localhost:8080` (published port): 3 of 6 runs hung until the timeout.
- From a container on the same Docker network as the server (no port forwarding): 6 of 6 runs passed in 4 to 6 seconds, each with only the one expected reset.

What to do:
- If only scenario 3 hangs or times out locally, don't try to fix the client for it. Rerun, or confirm with the same-network check below.
- To verify scenario 3 without the port forwarder, run the server and the client on a user-defined Docker network and address the server by container name:
  ```sh
  docker network create racenet
  docker run -d --rm --name easyracer --network racenet ghcr.io/jamesward/easyracer
  # run the client in a container on racenet, pointing it at http://easyracer:8080
  docker stop easyracer && docker network rm racenet
  ```
- After stopping a container, rootless Docker can briefly keep its host port bound. The next container start may then fail with `bind: address already in use`. Wait a few seconds and retry.
- Kill leftover test processes and stop leftover `ghcr.io/jamesward/easyracer` containers after an interrupted run. The `testcontainers/ryuk` container cleans up on its own.
