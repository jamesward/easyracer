import { NodeRuntime } from "@effect/platform-node"
import { program } from "./lib";

NodeRuntime.runMain(program(8080))
