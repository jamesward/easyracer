# AGENTS.md

Read `../AGENTS.md` first. It covers the repo layout and the rootless Docker problem that makes scenario 3 hang locally.

## Stack

- Effect 4 (`effect`, `@effect/platform-node`). Effect 4 is ESM-only, so this package is `"type": "module"` with `"module": "nodenext"`.
- TypeScript 7 (`tsc` is used only for type checking, never for emitting).
- Vitest 5 with `@effect/vitest`. Don't reintroduce Jest or ts-jest: ts-jest needs the TypeScript JS compiler API, which TypeScript 7 doesn't have.
- `tsx` runs the app (`npm run dev`).
- Node version comes from `engines.node` in `package.json`, which CI also uses.

## Commands

- `npm test`: type check, then run the scenarios against the server container (via Testcontainers, so Docker must be running).
- `npm run typecheck`: type check only.
- `npm run dev`: run `src/index.ts` against a server on `localhost:8080`.

## Conventions

- Each scenario stands alone. Don't pull shared request code out into helpers; duplicating it across scenarios is intended.
- Write effects with `Effect.gen`, not `flatMap`/`andThen` chains. Combinators like `Effect.race`, `Effect.timeout`, and `Effect.delay` via `.pipe` are fine.
- HTTP requests don't need `Effect.scoped`: in Effect 4, interrupting a request aborts it, so race losers are cancelled without a `Scope`.
- Relative imports use the `.js` extension (`./lib.js`), as `nodenext` requires.
- HTTP comes from `effect/http`. The Node client layer is `NodeHttpClient.layerNodeHttp` (`node:http`).
- In tests, keep `excludeTestServices: true` on the `layer(...)` block. Without it, `it.effect` uses `TestClock`, and the scenarios that rely on real delays and timeouts (4, 7, 10) stall.
