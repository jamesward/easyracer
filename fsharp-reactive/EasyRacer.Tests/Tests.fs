module EasyRacer.Tests

open DotNet.Testcontainers.Builders
open EasyRacer
open FSharp.Control.Reactive
open FSharpx.Control.Observable
open System
open System.Net
open System.Net.Http
open System.Net.Sockets
open Xunit

type TestcontainersFixture() =
    member this.container =
        ContainerBuilder()
            .WithImage("ghcr.io/jamesward/easyracer")
            .WithPortBinding(this.port)
            .WithWaitStrategy(Wait.ForUnixContainer().UntilHttpRequestIsSucceeded(fun r -> r.ForPort(uint16 this.port)))
            .Build()

    member this.port: int = 8080
    // Does not work
    // this.container.GetMappedPublicPort(8080)

    interface IAsyncLifetime with
        member this.InitializeAsync() = this.container.StartAsync()

        member this.DisposeAsync() = this.container.DisposeAsync().AsTask()

type ScenarioTests(fixture: TestcontainersFixture) =
    interface IClassFixture<TestcontainersFixture>

    static member scenarios = seq { for i in 1 .. Seq.length scenarios -> [| i |]: Object[] }

    [<Theory>]
    [<MemberData(nameof (scenarios))>]
    member this.``Response should be "right" for scenario``(num: int) =
        async {
            use http = new HttpClient()
            let baseUrl = $"http://localhost:{fixture.port}"

            let scenarioGet (path: string) : IObservable<HttpResponseMessage> =
                Observable.usingAsync
                    (fun cancellation -> http.GetAsync(baseUrl + path, cancellation) |> Async.AwaitTask)
                    (fun resp _ -> async { return Observable.single resp })

            let! actual = Array.get scenarios (num - 1) scenarioGet |> Async.AwaitObservable

            Assert.Equal("right", actual)
        }
