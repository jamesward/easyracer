open Lwt.Syntax
open Cohttp_lwt_unix
open Utils

let make_request_simple port ?query path =
  let uri =
    Uri.with_uri ~scheme:(Some "http") ~host:(Some "localhost")
      ~port:(Some port) ~path:(Some path) ~query Uri.empty
  in
  let* response, body = Client.get uri in
  let code = response |> Response.status |> Cohttp.Code.code_of_status in
  Lwt.return (code, body)

let make_request port ?query path =
  Lwt.catch
    (fun () ->
      let* code, body = make_request_simple port ?query path in
      match code with
      | 200 ->
          let* body = body |> Cohttp_lwt.Body.to_string in
          Lwt.return body
      | _ ->
          let wait_forever, _ = Lwt.task () in
          wait_forever)
    (fun _exn ->
      let wait_forever, _ = Lwt.task () in
      wait_forever)

let scenario_1 port =
  (* Race 2 concurrent requests. *)
  let path = "/1" in
  let request1 = make_request port path in
  let request2 = make_request port path in
  Lwt_main.run (Lwt.pick [ request1; request2 ])

let scenario_2 port =
  (* Race 2 concurrent requests, where one produces a connection error *)
  let path = "/2" in
  let request1 = make_request port path in
  let request2 = make_request port path in
  Lwt_main.run (Lwt.pick [ request1; request2 ])

let scenario_3 port =
  (* Race 10000 concurrent requests *)
  let path = "/3" in
  let counts = List.init 10000 (fun i -> i) in
  (* NOTE: Allow more than 10000 files to be opened: ulimit -n 12000 *)
  let requests = List.map (fun _ -> make_request port path) counts in
  Lwt_main.run (Lwt.pick requests)

let scenario_4 port =
  (* Race 2 concurrent requests but 1 of them should have a 1 second timeout *)
  let path = "/4" in
  let request1 = make_request port path in
  let request2 = make_request port path in
  let timeout =
    let* () = Lwt_unix.sleep 1.0 in
    Lwt.cancel request1;
    let wait_forever, _ = Lwt.task () in
    wait_forever
  in
  let request1_with_timeout = Lwt.pick [ request1; timeout ] in
  Lwt_main.run (Lwt.pick [ request1_with_timeout; request2 ])

let scenario_5 port =
  (* Race 2 concurrent requests where a non-200 response is a loser *)
  let path = "/5" in
  let request1 = make_request port path in
  let request2 = make_request port path in
  Lwt_main.run (Lwt.pick [ request1; request2 ])

let scenario_6 port =
  (* Race 3 concurrent requests where a non-200 response is a loser *)
  let path = "/6" in
  let request1 = make_request port path in
  let request2 = make_request port path in
  let request3 = make_request port path in
  Lwt_main.run (Lwt.pick [ request1; request2; request3 ])

let scenario_7 port =
  (* Start a request, wait at least 3 seconds then start a second request (hedging) *)
  let path = "/7" in
  let request1 = make_request port path in
  let hedge =
    let* () = Lwt_unix.sleep 3. in
    make_request port path
  in
  Lwt_main.run (Lwt.pick [ request1; hedge ])

let scenario_8 port =
  (* Race 2 concurrent requests that "use" a resource which is obtained and
     released through other requests *)
  let path = "/8" in
  let make_resource_request () =
    let* code = make_request ~query:[ ("open", [ "" ]) ] port path in
    let* status_code, body =
      make_request_simple ~query:[ ("use", [ code ]) ] port path
    in
    let* _ = make_request ~query:[ ("close", [ code ]) ] port path in
    let response =
      match status_code with
      | 200 -> body |> Cohttp_lwt.Body.to_string
      | _ ->
          let wait_forever, _ = Lwt.task () in
          wait_forever
    in
    response
  in
  let request1 = make_resource_request () in
  let request2 = make_resource_request () in
  Lwt_main.run (Lwt.pick [ request1; request2 ])

let scenario_9 port =
  (* Make 10 concurrent requests where 5 return a 200 response with a letter *)
  let path = "/9" in
  let rec wait_for n promises values =
    if n > 0 then
      let* vals, pending = Lwt.nchoose_split promises in
      wait_for (n - 1) pending (values @ vals)
    else Lwt.return values
  in
  let counts = List.init 10 (fun i -> i) in
  let requests = List.map (fun _ -> make_request port path) counts in
  Lwt_main.run (wait_for 5 requests []) |> String.concat ""

let scenario_10 port =
  (* Computationally heavy task run in parallel to another task, and then cancelled. *)
  let path = "/10" in
  Random.self_init ();
  let some_id = random_string 12 in

  (* Part 1 *)
  let query = [ (some_id, [ "" ]) ] in
  let request = make_request port ~query path in
  let computation = heavy_computation () in
  let part1 =
    let _ = Lwt.pick [ computation; request ] in
    Lwt.return_unit
  in

  (* Part 2 *)
  let rec send_load t ptime =
    let t_ = Unix.gettimeofday () in
    let elapsed = t_ -. t in
    let ptime_ = get_cpu_times () in
    let load =
      if elapsed > 0. then (ptime_ -. ptime) /. elapsed |> string_of_float
      else "1.0"
    in
    let query = [ (some_id, [ load ]) ] in
    let* code, _ = make_request_simple port ~query path in
    match code / 100 with
    | 2 -> Lwt.return_unit
    | 3 ->
        let* () = Lwt_unix.sleep 1.0 in
        send_load t_ ptime_
    | _ -> Lwt.return_unit
  in

  let t = Unix.gettimeofday () in
  let process_time = 0. in

  let scenario =
    let* _ = Lwt.join [ part1; send_load t process_time ] in
    Lwt.return "done"
  in

  Lwt_main.run scenario
