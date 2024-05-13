import { Effect } from "effect"
import {program} from "./lib";

Effect.runPromise(program(8080)).then(console.log)
