Elm with Platform.worker
------------------------

Elm implementation using "[The Elm Architecture](https://guide.elm-lang.org/architecture/)" (TEA).

TEA is Elm's built-in effect manager, for managing effects, such as the EasyRacer HTTP requests.

Elm is typically used for building web applications that run in the browser.
As EasyRacer is headless,
[`Platform.worker`](https://package.elm-lang.org/packages/elm/core/latest/Platform#worker)
allows us to use all of TEA's goodness without needing a web UI. 

Run the tests (using a Testcontainers scenario server):
```
elm make --optimize --output=app/EasyRacer.js src/EasyRacer.elm
```