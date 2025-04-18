(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Because our tests might be ran in the same process as other tests,
    we need to run our tests in a child process or next tests ran
    will fail if using [Unix.fork].
*)
let tztest label fn =
  Tztest.tztest label `Quick @@ fun () ->
  match Lwt_unix.fork () with
  | 0 -> (
      match Tezos_base_unix.Event_loop.main_run ~eio:true fn with
      | Ok () -> exit 0
      | Error _ -> exit 1)
  | pid -> (
      let open Lwt_result_syntax in
      let*! _, status = Lwt_unix.waitpid [] pid in
      match status with
      | Unix.WEXITED 0 -> return_unit
      | _ -> Lwt.return_error [])

let tests_fibonacci =
  let test_fibonacci domains =
    let fib () =
      match Tezos_bees.Task_worker.worker "worker" domains with
      | Error e -> Lwt_result_syntax.tzfail e
      | Ok worker ->
          let rec fib n = if n <= 1 then n else fib (n - 1) + fib (n - 2) in
          let input = Stdlib.List.init 5 (fun i -> i + 10) in
          let expected = List.map fib input in
          let* output =
            Tezos_bees.Task_worker.launch_tasks worker "fib" fib input
          in
          let output = List.filter_map Result.to_option output in
          Assert.equal_list expected output ;
          Lwt.return_ok ()
    in
    let name = Printf.sprintf "Fibonacci[11;12;13;14;15] %d domains" domains in
    tztest name fib
  in
  ("Fibonacci", [test_fibonacci 1; test_fibonacci 2; test_fibonacci 3])

let tests_reuse =
  (* Check that the same task worker can be used with totally different tasks and types. *)
  let test =
    tztest "reuse" @@ fun () ->
    match Tezos_bees.Task_worker.worker "worker" 1 with
    | Error e -> Lwt_result_syntax.tzfail e
    | Ok worker ->
        let int_input = 0 in
        let int_expected = succ int_input in
        let int_output =
          Tezos_bees.Task_worker.launch_task worker "succ(int)" succ int_input
        in
        let str_input = "0" in
        let succ s = int_of_string s |> succ |> string_of_int in
        let str_expected = succ str_input in
        let str_output =
          Tezos_bees.Task_worker.launch_task worker "succ(str)" succ str_input
        in
        let* int_output in
        let* str_output in
        Assert.equal (Ok int_expected) int_output ;
        Assert.equal (Ok str_expected) str_output ;
        Lwt.return_ok ()
  in
  ("Reuse a worker for a different kind of computation", [test])

let () =
  Alcotest_lwt.run ~__FILE__ "Task worker" [tests_fibonacci; tests_reuse]
  |> Lwt_main.run
