open Eio.Std
open Cohttp_eio
open Utils

type response = Http.Status.t * string option

exception Success of response

(* Wrap a function to raise a Success exception if it finishes successfully *)
let wrap_success f () =
  match f () with
  | x -> raise (Success x)
  | exception Eio.Cancel.Cancelled _ -> ()
  | exception e -> traceln "%a" Eio.Exn.pp e

(* Run a list of functions until one of them succeeds *)
let any_success fns =
  try
    Fiber.all (List.map wrap_success fns);
    failwith "All failed!"
  with Success x -> x

let make_request ~port ~path ?query ~net () =
  let uri =
    Uri.with_uri ~scheme:(Some "http") ~host:(Some "localhost")
      ~port:(Some port) ~path:(Some path) ~query Uri.empty
  in
  let client = Client.make ~https:None net in
  Eio.Switch.run @@ fun sw ->
  let resp, body = Client.get client ~sw uri in
  if Http.Status.compare resp.status `OK = 0 then
    let content = Eio.Buf_read.(parse_exn take_all) body ~max_size:max_int in
    (resp.status, Some content)
  else (resp.status, None)

let make_request_check_status ~port ~path ?query ~net () =
  let ((status, _) as r) = make_request ~port ~path ?query ~net () in
  match status with
  | `OK -> r
  | _ ->
      failwith
      @@ Printf.sprintf "Request status code: %s"
      @@ Http.Status.to_string status

let scenario_1 port =
  (* Race 2 concurrent requests. *)
  let path = "/1" in
  Eio_main.run @@ fun env ->
  let net = env#net in
  let request1 = make_request ~port ~path ~net in
  let request2 = make_request ~port ~path ~net in
  let _, body = any_success [ request1; request2 ] in
  match body with Some c -> c | None -> "empty"

let scenario_2 port =
  (* Race 2 concurrent requests, where one produces a connection error *)
  let path = "/2" in
  Eio_main.run @@ fun env ->
  let net = env#net in
  let request1 = make_request ~port ~path ~net in
  let request2 = make_request ~port ~path ~net in
  let _, body = any_success [ request1; request2 ] in
  match body with Some c -> c | None -> "empty"

let scenario_3 port =
  (* Race 10000 concurrent requests *)
  let path = "/3" in
  Eio_main.run @@ fun env ->
  let net = env#net in
  let counts = List.init 10000 (fun i -> i) in
  (* NOTE: Allow more than 10000 files to be opened: ulimit -n 12000 *)
  let requests =
    List.map (fun _ () -> make_request ~port ~path ~net ()) counts
  in
  let _, body = any_success requests in
  match body with Some c -> c | None -> "empty"

let scenario_4 port =
  (* Race 2 concurrent requests but 1 of them should have a 1 second timeout *)
  let path = "/4" in
  let timeout = 1. in
  Eio_main.run @@ fun env ->
  let net = env#net in
  let clock = env#clock in
  let request_with_timeout () =
    Eio.Time.with_timeout_exn clock timeout (fun () ->
        make_request ~port ~path ~net ())
  in
  let request () = make_request ~port ~path ~net () in
  let _, body = any_success [ request; request_with_timeout ] in
  match body with Some c -> c | None -> "empty"

let scenario_5 port =
  (* Race 2 concurrent requests where a non-200 response is a loser *)
  let path = "/5" in
  Eio_main.run @@ fun env ->
  let net = env#net in
  let request1 = make_request_check_status ~port ~path ~net in
  let request2 = make_request_check_status ~port ~path ~net in
  let _, body = any_success [ request1; request2 ] in
  match body with Some c -> c | None -> "empty"

let scenario_6 port =
  (* Race 3 concurrent requests where a non-200 response is a loser *)
  let path = "/6" in
  Eio_main.run @@ fun env ->
  let net = env#net in
  let request1 = make_request_check_status ~port ~path ~net in
  let request2 = make_request_check_status ~port ~path ~net in
  let request3 = make_request_check_status ~port ~path ~net in
  let _, body = any_success [ request1; request2; request3 ] in
  match body with Some c -> c | None -> "empty"

let scenario_7 port =
  (* Start a request, wait at least 3 seconds then start a second request (hedging) *)
  let path = "/7" in
  Eio_main.run @@ fun env ->
  let net = env#net in
  let clock = env#clock in
  let request1 = make_request_check_status ~port ~path ~net in
  let request2 () =
    Eio.Time.sleep clock 3.;
    make_request_check_status ~port ~path ~net ()
  in
  let _, body = any_success [ request1; request2 ] in
  match body with Some c -> c | None -> "empty"

let scenario_8 port =
  (* Race 2 concurrent requests that "use" a resource which is obtained and
     released through other requests *)
  let path = "/8" in
  Eio_main.run @@ fun env ->
  let net = env#net in
  let make_resource_request () =
    let _, content =
      make_request ~port ~path ~query:[ ("open", [ "" ]) ] ~net ()
    in
    let code = match content with Some code -> code | None -> "no-code" in
    let use_response =
      make_request ~port ~path ~query:[ ("use", [ code ]) ] ~net ()
    in
    let _ = make_request ~port ~path ~query:[ ("close", [ code ]) ] ~net () in
    match use_response with
    | `OK, _ -> use_response
    | _ -> failwith "Use request failed"
  in
  let request1 = make_resource_request in
  let request2 = make_resource_request in
  let _, body = any_success [ request1; request2 ] in
  match body with Some c -> c | None -> "empty"

let scenario_9 port =
  (* Make 10 concurrent requests where 5 return a 200 response with a letter *)
  let path = "/9" in
  Eio_main.run @@ fun env ->
  let counts = List.init 10 (fun i -> i) in
  let net = env#net in
  let responses = ref "" in
  let requests =
    List.map
      (fun _ () ->
        match make_request ~port ~path ~net () with
        | `OK, Some s -> responses := !responses ^ s
        | _ -> ())
      counts
  in
  Fiber.all requests;
  !responses

let scenario_10 port =
  (* Computationally heavy task run in parallel to another task, and then cancelled. *)
  let path = "/10" in

  Eio_main.run @@ fun env ->
  let net = env#net in
  let clock = env#clock in
  (* Part 1 *)
  Random.self_init ();
  let some_id = random_string 12 in
  let query = [ (some_id, [ "" ]) ] in
  let request = make_request ~port ~query ~path ~net in
  let part1 () =
    let _ = any_success [ request; heavy_computation ] in
    ()
  in

  (* Part 2 *)
  let rec send_load t ptime () =
    let t_ = Unix.gettimeofday () in
    let elapsed = t_ -. t in
    let ptime_ = get_cpu_times () in
    let load =
      if elapsed > 0. then (ptime_ -. ptime) /. elapsed |> string_of_float
      else "1.0"
    in
    let query = [ (some_id, [ load ]) ] in
    let code, _ = make_request ~port ~path ~query ~net () in
    match Http.Status.to_int code / 100 with
    | 2 -> ()
    | 3 ->
        Eio.Switch.run @@ fun sw ->
        Fiber.fork ~sw (fun () ->
            Eio.Time.sleep clock 1.;
            send_load t_ ptime_ ())
    | _ -> ()
  in

  let t = Unix.gettimeofday () in
  let process_time = 0. in
  Fiber.all [ part1; send_load t process_time ];
  "done"
