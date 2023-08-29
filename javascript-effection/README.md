## EasyRacer: Javascript + Effection

This implementation uses [Effection v3](https://effection.deno.dev)

In order to run the tests, you will first need to run the scenario server, so
see the [main readme](../README.md) for how to do that.

### Deno

make sure you have [Deno](https://deno.land/x) installed.

```shellsession
$ deno task test
```

### Node

You will need [Node](https://nodejs.org) >= 18 installed. If you are using,
[Volta](https://volta.sh) this will be configured for you automatically.

``` shellsession
$ npm install
$ npx tsx --test easyracer.test.ts
```
