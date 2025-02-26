(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Functori, <contact@functori.com>                       *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

type global = {data_dir : string; verbosity : Internal_event.level}

let default_data_dir = Filename.concat (Sys.getenv "HOME") ".outbox-monitor"

let default_global = {data_dir = default_data_dir; verbosity = Notice}

module Parameter = struct
  let endpoint =
    Tezos_clic.parameter (fun _ uri -> Lwt.return_ok (Uri.of_string uri))
end

module Arg = struct
  let verbose =
    Tezos_clic.switch
      ~short:'v'
      ~long:"verbose"
      ~doc:"Sets logging level to info"
      ()

  let debug =
    Tezos_clic.switch ~long:"debug" ~doc:"Sets logging level to debug" ()

  let endpoint ~default ~long ~doc =
    Tezos_clic.default_arg
      ~default
      ~long
      ~doc
      ~placeholder:"URL"
      Parameter.endpoint

  let evm_node_endpoint =
    endpoint
      ~default:"http://127.0.0.1:8545/ws"
      ~long:"evm-node"
      ~doc:"Websocket endpoint to reach EVM node"

  let data_dir =
    Tezos_clic.arg
      ~long:"data-dir"
      ~short:'d'
      ~doc:
        (Format.sprintf
           "Directory where data is stored for the outbox monitor.\n\
            Defaults to `%s`."
           default_data_dir)
      ~placeholder:"path"
    @@ Tezos_clic.parameter (fun _ x -> Lwt_result.return x)
end

let log_config ~verbosity () =
  let open Tezos_base_unix.Internal_event_unix in
  let config =
    make_with_defaults
      ~verbosity
      ~log_cfg:
        (Tezos_base_unix.Logs_simple_config.create_cfg
           ~advertise_levels:true
           ())
      ()
  in
  init ~config ()

let global_options_args =
  let open Tezos_clic in
  let open Arg in
  map_arg
    (aggregate (args3 data_dir verbose debug))
    ~f:(fun g (data_dir, verbose, debug) ->
      let verbosity =
        if debug then Internal_event.Debug
        else if verbose then Info
        else g.verbosity
      in
      let data_dir = Option.value data_dir ~default:g.data_dir in
      Lwt_result.return {data_dir; verbosity})

let global_options = Tezos_clic.args1 global_options_args

let om_command ~desc args cmd_prefixes f =
  let open Tezos_clic in
  command
    ~desc
    (args2 global_options_args (aggregate args))
    cmd_prefixes
    (fun (global, args) -> f global args)

let run_command =
  let open Tezos_clic in
  om_command
    ~desc:"Start monitoring outbox"
    (args1 Arg.evm_node_endpoint)
    (prefixes ["run"] @@ stop)
    (fun {data_dir; verbosity} evm_node_endpoint _ ->
      let open Lwt_result_syntax in
      let*! () = log_config ~verbosity () in
      let* _db = Db.init ~data_dir `Read_write in
      let*! ws_client =
        Websocket_client.connect Media_type.json evm_node_endpoint
      in
      let* heads_subscription = Websocket_client.subscribe_newHeads ws_client in
      let cpt = ref 0 in
      let*! () =
        Lwt_stream.iter_s
          (fun (head : _ Ethereum_types.block) ->
            incr cpt ;
            Format.eprintf "Block %a@." Ethereum_types.pp_quantity head.number ;
            if !cpt = 10 then
              let*! _ = heads_subscription.unsubscribe () in
              Lwt.return_unit
            else Lwt.return_unit)
          heads_subscription.stream
      in
      return_unit)

let commands = [run_command]

let executable_name = Filename.basename Sys.executable_name

let dispatch args =
  let open Lwt_result_syntax in
  let commands =
    Tezos_clic.add_manual
      ~executable_name
      ~global_options
      (if Unix.isatty Unix.stdout then Tezos_clic.Ansi else Tezos_clic.Plain)
      Format.std_formatter
      commands
  in
  let* global, remaining_args =
    Tezos_clic.parse_global_options global_options default_global args
  in
  Tezos_clic.dispatch commands global remaining_args

let handle_error = function
  | Ok _ -> ()
  | Error [Tezos_clic.Version] ->
      Format.printf
        "outbox-monitor.%s (%s)@."
        Tezos_version_value.Current_git_info.abbreviated_commit_hash
        Tezos_version_value.Current_git_info.committer_date ;
      exit 0
  | Error [Tezos_clic.Help command] ->
      Tezos_clic.usage
        Format.std_formatter
        ~executable_name
        ~global_options
        (match command with None -> [] | Some c -> [c]) ;
      Stdlib.exit 0
  | Error errs ->
      Tezos_clic.pp_cli_errors
        Format.err_formatter
        ~executable_name
        ~global_options
        ~default:Error_monad.pp
        errs ;
      Stdlib.exit 1

let argv () = Array.to_list Sys.argv |> List.tl |> Stdlib.Option.get

let () =
  Random.self_init () ;
  let _ =
    Tezos_clic.(
      setup_formatter
        Format.err_formatter
        (if Unix.isatty Unix.stderr then Ansi else Plain)
        Short)
  in
  let _ =
    Tezos_clic.(
      setup_formatter
        Format.std_formatter
        (if Unix.isatty Unix.stdout then Ansi else Plain)
        Short)
  in
  Lwt.Exception_filter.(set handle_all_except_runtime) ;
  Tezos_base_unix.Event_loop.main_run
    (Lwt_exit.wrap_and_exit (dispatch (argv ())))
  |> handle_error
