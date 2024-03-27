open Ocaml_cohttp_eio

let () =
  let port = 8080 in
  let scenarios =
    [
      Scenarios.scenario_1;
      Scenarios.scenario_2;
      Scenarios.scenario_3;
      Scenarios.scenario_4;
      Scenarios.scenario_5;
      Scenarios.scenario_6;
      Scenarios.scenario_7;
      Scenarios.scenario_8;
      Scenarios.scenario_9;
      Scenarios.scenario_10;
    ]
  in
  Printf.printf "\nRunning All Scenarios\n\n";
  List.iteri
    (fun i scenario ->
      Printf.printf "Running scenario %d\n" (i + 1);
      let response = scenario port in
      Printf.printf "Response: %s\n\n" response)
    scenarios
