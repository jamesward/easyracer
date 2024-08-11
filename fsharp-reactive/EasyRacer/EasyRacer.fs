open FSharp.Control.Reactive
open FSharpx.Control.Observable
open System
open System.Diagnostics
open System.Net
open System.Net.Http
open System.Security.Cryptography

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
        Observable.delay (TimeSpan.FromSeconds 3) (Observable.defer (fun () -> req))

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

let scenario10 (scenarioGet: string -> IObservable<HttpResponseMessage>) : IObservable<string> =
    let id = Guid.NewGuid()
    let sha512 = SHA512.Create()
    let random = Random()
    let proc = Process.GetCurrentProcess()

    let blocking =
        Observable.repeatValue ()
        |> Observable.scanInit
            [| for _ in 0..512 do
                   yield (byte (random.Next 256)) |]
            (fun bytes _ -> sha512.ComputeHash(bytes))
        |> Observable.map BitConverter.ToString

    let blocker =
        scenarioGet $"/10?{id}"
        |> Observable.filter (fun resp -> resp.IsSuccessStatusCode)
        |> Observable.bind (fun resp -> resp.Content.ReadAsStringAsync() |> Async.AwaitTask |> Observable.ofAsync)

    let rec reportProcessLoad (startWallTime: DateTime) (startCpuTime: TimeSpan) =
        let endWallTime = DateTime.Now
        let endCpuTime = proc.TotalProcessorTime

        let cpuLoad =
            endCpuTime.Add(startCpuTime.Negate()).TotalMilliseconds
            / endWallTime.Subtract(startWallTime).TotalMilliseconds
            * (double Environment.ProcessorCount)

        scenarioGet $"/10?{id}={cpuLoad}"
        |> Observable.bind (function
            | resp when resp.IsSuccessStatusCode ->
                resp.Content.ReadAsStringAsync() |> Async.AwaitTask |> Observable.ofAsync
            | resp when
                resp.StatusCode < HttpStatusCode.OK
                || resp.StatusCode >= HttpStatusCode.BadRequest
                ->
                Observable.single ($"bad HTTP status: {resp.StatusCode}")
            | resp ->
                Observable.delay
                    (TimeSpan.FromSeconds 1)
                    (Observable.defer (fun () -> reportProcessLoad endWallTime endCpuTime)))

    let reporter = reportProcessLoad DateTime.Now proc.TotalProcessorTime

    Observable.merge blocking blocker
    |> Observable.filter (fun text -> text = "")
    |> Observable.take 1
    |> Observable.merge (reporter)
    |> Observable.filter (fun text -> text <> "")
    |> Observable.take 1

let scenarios: ((string -> IObservable<HttpResponseMessage>) -> IObservable<string>) array =
    [| scenario1
       scenario2
       scenario3
       scenario4
       scenario5
       scenario6
       scenario7
       scenario8
       scenario9
       scenario10 |]

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
