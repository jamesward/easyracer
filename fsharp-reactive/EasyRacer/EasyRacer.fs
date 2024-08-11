open FSharp.Control.Reactive
open FSharpx.Control.Observable
open System
open System.Net.Http

let scenario1 (scenarioGet: string -> IObservable<HttpResponseMessage>) : IObservable<string> =
    let req =
        scenarioGet "/1"
        |> Observable.bind (fun resp -> resp.Content.ReadAsStringAsync() |> Async.AwaitTask |> Observable.ofAsync)

    Observable.merge req req |> Observable.take 1

let scenario2 (scenarioGet: string -> IObservable<HttpResponseMessage>) : IObservable<string> =
    let req =
        scenarioGet "/2"
        |> Observable.bind (fun resp -> resp.Content.ReadAsStringAsync() |> Async.AwaitTask |> Observable.ofAsync)
        |> Observable.catch Observable.empty

    Observable.merge req req |> Observable.take 1

let scenario3 (scenarioGet: string -> IObservable<HttpResponseMessage>) : IObservable<string> =
    let req =
        scenarioGet "/3"
        |> Observable.bind (fun resp -> resp.Content.ReadAsStringAsync() |> Async.AwaitTask |> Observable.ofAsync)

    Seq.replicate 10_000 req |> Observable.mergeSeq |> Observable.take 1

let scenario4 (scenarioGet: string -> IObservable<HttpResponseMessage>) : IObservable<string> =
    let req =
        scenarioGet "/4"
        |> Observable.bind (fun resp -> resp.Content.ReadAsStringAsync() |> Async.AwaitTask |> Observable.ofAsync)

    let reqWithTimeout =
        req
        |> Observable.timeoutSpan (TimeSpan.FromSeconds 1)
        |> Observable.take 1
        |> Observable.catch Observable.empty

    Observable.merge req reqWithTimeout |> Observable.take 1

let scenario5 (scenarioGet: string -> IObservable<HttpResponseMessage>) : IObservable<string> =
    let req =
        scenarioGet "/5"
        |> Observable.filter (fun resp -> resp.IsSuccessStatusCode)
        |> Observable.bind (fun resp -> resp.Content.ReadAsStringAsync() |> Async.AwaitTask |> Observable.ofAsync)

    Observable.merge req req |> Observable.take 1

let scenario6 (scenarioGet: string -> IObservable<HttpResponseMessage>) : IObservable<string> =
    let req =
        scenarioGet "/6"
        |> Observable.filter (fun resp -> resp.IsSuccessStatusCode)
        |> Observable.bind (fun resp -> resp.Content.ReadAsStringAsync() |> Async.AwaitTask |> Observable.ofAsync)

    Observable.mergeSeq [ req; req; req ] |> Observable.take 1

let scenario7 (scenarioGet: string -> IObservable<HttpResponseMessage>) : IObservable<string> =
    let req =
        scenarioGet "/7"
        |> Observable.filter (fun resp -> resp.IsSuccessStatusCode)
        |> Observable.bind (fun resp -> resp.Content.ReadAsStringAsync() |> Async.AwaitTask |> Observable.ofAsync)

    let reqWithDelay =
        Observable.delay (TimeSpan.FromSeconds 3) (Observable.single ())
        |> Observable.bind (fun () -> req)

    Observable.merge req reqWithDelay |> Observable.take 1

type ResourceFactory(resource: IObservable<String>, close: IObservable<String>) =
    member this.resource = resource

    interface IDisposable with
        member this.Dispose() =
            close |> Async.AwaitObservable |> Async.RunSynchronously |> ignore

let scenario8 (scenarioGet: string -> IObservable<HttpResponseMessage>) : IObservable<string> =
    let req path =
        scenarioGet path
        |> Observable.filter (fun resp -> resp.IsSuccessStatusCode)
        |> Observable.bind (fun resp -> resp.Content.ReadAsStringAsync() |> Async.AwaitTask |> Observable.ofAsync)

    let reqOpen = req "/8?open"
    let reqUse id = req $"/8?use={id}"
    let reqClose id = req $"/8?close={id}"

    let reqRes =
        Observable.usingAsync
            (fun _ ->
                reqOpen
                |> Observable.map (fun id -> new ResourceFactory(reqUse id, reqClose id))
                |> Async.AwaitObservable)
            (fun res _ -> async { return res.resource })

    Observable.merge reqRes reqRes |> Observable.take 1

let scenario9 (scenarioGet: string -> IObservable<HttpResponseMessage>) : IObservable<string> =
    let req =
        scenarioGet "/9"
        |> Observable.filter (fun resp -> resp.IsSuccessStatusCode)
        |> Observable.bind (fun resp -> resp.Content.ReadAsStringAsync() |> Async.AwaitTask |> Observable.ofAsync)

    Seq.replicate 10 req |> Observable.mergeSeq |> Observable.fold (+) ""

let scenarios: ((string -> IObservable<HttpResponseMessage>) -> IObservable<string>) array =
    [| scenario1
       scenario2
       scenario3
       scenario4
       scenario5
       scenario6
       scenario7
       scenario8
       scenario9 |]

[<EntryPoint>]
let main _ =
    use http = new HttpClient()
    let baseUrl = "http://localhost:8080"

    let scenarioGet (path: string) : IObservable<HttpResponseMessage> =
        Observable.usingAsync
            (fun cancellation -> http.GetAsync(baseUrl + path, cancellation) |> Async.AwaitTask)
            (fun resp _ -> async { return Observable.single resp })

    scenarios
    |> Seq.map ((|>) scenarioGet)
    |> Observable.concatSeq
    |> Observable.iter (printfn "%s")
    |> Observable.last
    |> Async.AwaitObservable
    |> Async.RunSynchronously
    |> ignore

    0
