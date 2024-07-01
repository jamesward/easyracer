Elm with Platform.worker
------------------------

Elm implementation using "[The Elm Architecture](https://guide.elm-lang.org/architecture/)" (TEA).

TEA is Elm's built-in effect manager, for managing effects, such as the EasyRacer HTTP requests.

Elm is typically used for building web applications that run in the browser.
As EasyRacer is headless,
[`Platform.worker`](https://package.elm-lang.org/packages/elm/core/latest/Platform#worker)
allows us to use all of TEA's goodness without needing a web UI. 

For scenario 10, we have to fallback to JavaScript for:

- Obtaining the process load
- The reporter HTTP requests, specifically to handle the 302 response, since the native Elm
  HTTP client _always_ follow redirects. 

Run the tests (using a Testcontainers scenario server):
```
for num in {1..10}; do
  elm make --optimize --output=app/EasyRacer/Scenario$num.js src/EasyRacer/Scenario$num.elm
done && npm test
```