open Alcotest
open Ocaml_cohttp_lwt

module Docker = struct
  let container_name = "easyracer_server"

  let run_docker_command cmd =
    let open Unix in
    let in_channel, out_channel, err_channel =
      open_process_full cmd (environment ())
    in
    let output = ref "" in
    let error = ref "" in

    (try
       while true do
         output := !output ^ input_line in_channel ^ "\n"
       done
     with End_of_file -> ());

    (try
       while true do
         error := !error ^ input_line err_channel ^ "\n"
       done
     with End_of_file -> ());

    let exit_status =
      close_process_full (in_channel, out_channel, err_channel)
    in
    match exit_status with
    | WEXITED code ->
        if code <> 0 then
          failwith
          @@ Printf.sprintf "Command failed with exit code: %d\nError: %s" code
               !error
        else !output
    | WSIGNALED _ | WSTOPPED _ ->
        failwith "Command was killed or stopped unexpectedly"

  (* Start the Docker container and expose a port *)
  let start_container port =
    let image_name = "ghcr.io/jamesward/easyracer:latest" in
    let docker_run_cmd =
      Printf.sprintf "docker run --rm -d -p %d:%d --name %s %s --debug" port
        port container_name image_name
    in
    let s = run_docker_command docker_run_cmd in
    print_endline s;
    (* Wait for 2 seconds, for simplicity *)
    Unix.sleep 2

  (* Stop and remove the Docker container *)
  let stop_container () =
    let docker_stop_cmd =
      Printf.sprintf "docker kill -s SIGKILL %s" container_name
    in
    let s = run_docker_command docker_stop_cmd in
    print_endline s
end

let port = 8080
let right = "right"

let test_scenario_1 () =
  let response = Scenarios.scenario_1 port in
  check string "Scenario 1 response" right response

let test_scenario_2 () =
  let response = Scenarios.scenario_2 port in
  check string "Scenario 2 response" right response

let test_scenario_3 () =
  let response = Scenarios.scenario_3 port in
  check string "Scenario 3 response" right response

let test_scenario_4 () =
  let response = Scenarios.scenario_4 port in
  check string "Scenario 4 response" right response

let test_scenario_5 () =
  let response = Scenarios.scenario_5 port in
  check string "Scenario 5 response" right response

let test_scenario_6 () =
  let response = Scenarios.scenario_6 port in
  check string "Scenario 6 response" right response

let test_scenario_7 () =
  let response = Scenarios.scenario_7 port in
  check string "Scenario 7 response" right response

let test_scenario_8 () =
  let response = Scenarios.scenario_8 port in
  check string "Scenario 8 response" right response

let test_scenario_9 () =
  let response = Scenarios.scenario_9 port in
  check string "Scenario 9 response" right response

let test_scenario_10 () =
  let response = Scenarios.scenario_10 port in
  check string "Scenario 10 response" "done" response

let teardown = Docker.stop_container

let run_tests () =
  run "Scenarios"
    [
      ("Scenario 1", [ test_case "Test race" `Quick test_scenario_1 ]);
      ("Scenario 2", [ test_case "Test race with error" `Quick test_scenario_2 ]);
      ("Scenario 3", [ test_case "Test race 10k" `Slow test_scenario_3 ]);
      ( "Scenario 4",
        [ test_case "Test race with timeout" `Slow test_scenario_4 ] );
      ( "Scenario 5",
        [ test_case "Test race non-200 response" `Quick test_scenario_5 ] );
      ( "Scenario 6",
        [
          test_case "Test race non-200 response (3 requests)" `Quick
            test_scenario_6;
        ] );
      ("Scenario 7", [ test_case "Test 3 sec hedge" `Slow test_scenario_7 ]);
      ( "Scenario 8",
        [ test_case "Test using a resource" `Quick test_scenario_8 ] );
      ( "Scenario 9",
        [ test_case "Test 5 requests with a letter" `Slow test_scenario_9 ] );
      ( "Scenario 10",
        [
          test_case "Test computationally heavy task with cancellation" `Slow
            test_scenario_10;
        ] );
      (* HACK: Ensure docker container is always shut down *)
      ("Teardown", [ test_case "Tear down" `Slow teardown ]);
    ]

let () =
  Docker.start_container port;
  run_tests ()
