import {program} from "./lib.js";
import {Effect} from "effect";

Effect.runPromise(program(8080)).then(console.log)
