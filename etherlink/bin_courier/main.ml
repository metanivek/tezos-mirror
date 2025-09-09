(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Evm_node_lib_dev
open Floodgate_lib
open Evm_node_lib_dev_encoding

module Events = struct
  open Internal_event.Simple

  let section = ["courier"]

  let accepted_transaction =
    declare_1
      ~section
      ~name:"accepted_transaction"
      ~msg:"transaction {hash} was accepted by the sequencer"
      ~level:Notice
      ~pp1:Ethereum_types.pp_hash
      ("hash", Ethereum_types.hash_encoding)

  let confirmed_withdrawal =
    declare_0
      ~section
      ~name:"confirmed_withdrawal"
      ~msg:"withdrawal was included in a block"
      ~level:Notice
      ()

  let accepted_transaction hash = emit accepted_transaction hash

  let confirmed_withdrawal = emit confirmed_withdrawal
end

module Network = struct
  type t = Mainnet | Testnet

  let relay network =
    Uri.of_string
    @@
    match network with
    | Mainnet -> "https://relay.mainnet.etherlink.com"
    | Testnet -> "https://relay.ghostnet.etherlink.com"

  let endpoint network =
    Uri.of_string
    @@
    match network with
    | Mainnet -> "https://node.mainnet.etherlink.com"
    | Testnet -> "https://node.ghostnet.etherlink.com"
end

open Network

let is_number n =
  match Z.of_string n with _ -> true | exception Invalid_argument _ -> false

let is_mutez n = is_number n && 0 < String.length n && String.length n <= 6

let of_tez_string tez =
  let pow_ten n = Z.(of_int 10 ** n) in
  let open Result_syntax in
  match String.split_on_char '.' (String.trim tez) with
  | [eth] when is_number eth -> return Z.(of_string eth * pow_ten 18)
  | [eth; decimal] when is_number eth && is_mutez decimal ->
      let eth = Z.(of_string eth * pow_ten 18) in
      let decimal = decimal ^ String.make (6 - String.length decimal) '0' in
      let decimal = Z.(of_string decimal * pow_ten 12) in
      return (Z.add eth decimal)
  | [eth; decimal] when is_number eth && not (is_mutez decimal) ->
      error_with "%s has too many decimals" tez
  | _ -> error_with "%s is not a valid tez amount" tez

let signer_from_string value =
  let open Lwt_result_syntax in
  match Configuration.gcp_key_from_string_opt value with
  | None ->
      let*? secret_key = Signer.secret_key_from_hex (`Hex value) in
      return (Signer.from_secret_key secret_key)
  | Some key ->
      Signer.from_gcp_key
        {
          pool_size = 1;
          authentication_method = Gcloud_auth;
          authentication_retries = 2;
          authentication_frequency_min = 30;
          authentication_retry_backoff_sec = 5;
          authentication_timeout_sec = 5;
          gcloud_path = "gcloud";
        }
        key

module Parameter = struct
  let tez = Tezos_clic.parameter (fun _ n -> Lwt.return (of_tez_string n))

  let network =
    let open Lwt_result_syntax in
    Tezos_clic.parameter (fun _ -> function
      | "mainnet" -> return Mainnet
      | "testnet" -> return Testnet
      | n ->
          failwith
            "%s is not a known network (expected one of: 'mainnet', 'testnet', \
             braeburn', 'rainbow')"
            n)

  let signer =
    Tezos_clic.parameter (fun _ value ->
        match String.remove_prefix ~prefix:"env://" value with
        | Some env -> signer_from_string (Sys.getenv env)
        | None -> signer_from_string value)

  let receiver =
    Tezos_clic.parameter (fun _ str ->
        Lwt.return (Tezos_crypto.Signature.Public_key_hash.of_b58check str))
end

module Arg = struct
  let network =
    Tezos_clic.default_arg
      ~default:"testnet"
      ~env:"NETWORK"
      ~long:"network"
      ~doc:"Etherlink network"
      ~placeholder:"NETWORK"
      Parameter.network
end

module Constant = struct
  let withdrawal_contract =
    Efunc_core.Types.a "ff00000000000000000000000000000000000001"
end

let start_blueprint_follower ~relay_endpoint =
  let open Lwt_result_syntax in
  let open Floodgate_lib in
  let* next_blueprint_number =
    Batch.call
      (module Rpc_encodings.Block_number)
      ~keep_alive:true
      ~evm_node_endpoint:relay_endpoint
      ()
  in
  let* time_between_blocks =
    Evm_services.get_time_between_blocks
      ~fallback:(Time_between_blocks 10.)
      ~evm_node_endpoint:relay_endpoint
      ()
  in
  Blueprints_follower.start
    ~multichain:false
    ~time_between_blocks
    ~evm_node_endpoint:relay_endpoint
    ~next_blueprint_number
    ~on_new_blueprint:(fun number blueprint ->
      let*! () = Floodgate_events.received_blueprint number in
      let* () =
        match Blueprint_decoder.transaction_hashes blueprint with
        | Ok hashes -> List.iter_es Tx_queue.confirm hashes
        | Error _ -> return_unit
      in
      return `Continue)
    ~on_finalized_levels:(fun ~l1_level:_ ~start_l2_level:_ ~end_l2_level:_ ->
      return_unit)
    ()

let withdraw_data receiver =
  let receiver = Tezos_crypto.Signature.Public_key_hash.to_b58check receiver in
  Efunc_core.Evm.encode ~name:"withdraw_base58" [`string] [`string receiver]

let withdraw ~endpoint ~infos value from receiver =
  let open Lwt_result_syntax in
  let result, rwaker = Lwt.task () in
  let hash, hwaker = Lwt.task () in
  let*! () =
    Tx_queue.transfer
      ~callback:
        (let open Lwt_syntax in
         function
         | `Confirmed ->
             let* () = Events.confirmed_withdrawal () in
             Lwt.wakeup rwaker (Ok ()) ;
             return_unit
         | `Refused ->
             Lwt.wakeup rwaker (error_with "Withdrawal refused") ;
             return_unit
         | `Dropped ->
             Lwt.wakeup rwaker (error_with "Withdrawal dropped") ;
             return_unit
         | `Accepted hash ->
             Lwt.wakeup hwaker hash ;
             Events.accepted_transaction hash)
      ~gas_limit:(Z.of_int 16_150_912)
      ~infos
      ~from
      ~to_:Constant.withdrawal_contract
      ~value
      ~data:(withdraw_data receiver)
      ()
  in
  let* () = result in
  let*! hash in
  let* receipt_rpc_result =
    Batch.call
      (module Rpc_encodings.Get_transaction_receipt)
      ~keep_alive:true
      ~evm_node_endpoint:endpoint
      hash
  in
  match receipt_rpc_result with
  | None -> failwith "Could not find the receipt"
  | Some receipt ->
      let (Qty status) = receipt.status in
      if Z.(equal status Z.one) then return_unit
      else failwith "The withdraw failed"

let command =
  let open Tezos_clic in
  command
    ~desc:"Withdraw funds from an Etherlink network"
    Arg.(args1 network)
    (prefix "withdraw"
    @@ param ~name:"amount" ~desc:"Amount of tez to withdraw" Parameter.tez
    @@ prefix "from"
    @@ param
         ~name:"source"
         ~desc:
           "Source of the transaction (either a hexadecimal-encoded private \
            key or a GCP KMS key handler). It is also possible to use \
            `env://VAR` to have the node read this parameter from an \
            environment variable, notably to avoid having private keys leaking \
            in the shell history."
         Parameter.signer
    @@ prefix "to"
    @@ param
         ~name:"receiver"
         ~desc:
           "B58checked encoded public key hash of the receiver on the Layer 1"
         Parameter.receiver
    @@ stop)
    (fun network amount signer receiver _ ->
      let open Lwt_result_syntax in
      let* () =
        Tx_queue.start
          ~relay_endpoint:(Network.relay network)
          ~max_transaction_batch_length:None
          ~inclusion_timeout:5.
          ()
      in

      let _ =
        start_blueprint_follower ~relay_endpoint:(Network.relay network)
      in
      let _ = Tx_queue.beacon ~tick_interval:0.1 in
      let* infos =
        Floodgate_lib.Network_info.fetch
          ~rpc_endpoint:(Network.endpoint network)
          ~base_fee_factor:1.
      in
      let* from =
        Account.from_signer ~evm_node_endpoint:(Network.endpoint network) signer
      in
      withdraw ~endpoint:(Network.endpoint network) ~infos amount from receiver)

let global_options = Tezos_clic.no_options

let executable_name = Filename.basename Sys.executable_name

let dispatch args =
  let open Lwt_result_syntax in
  let commands =
    Tezos_clic.add_manual
      ~executable_name
      ~global_options
      (if Unix.isatty Unix.stdout then Tezos_clic.Ansi else Tezos_clic.Plain)
      Format.std_formatter
      [command]
  in
  let* (), remaining_args =
    Tezos_clic.parse_global_options global_options () args
  in
  Tezos_clic.dispatch ~enable_argDefSwitch:true commands () remaining_args

let handle_error = function
  | Ok _ -> ()
  | Error [Tezos_clic.Version] ->
      let devmode =
        Tezos_version_value.Bin_version.octez_evm_node_version_string
      in
      Format.printf "%s\n" devmode ;
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

let init_log () =
  let log_cfg =
    Tezos_base_unix.Logs_simple_config.create_cfg ~advertise_levels:true ()
  in
  Tezos_base_unix.Internal_event_unix.init
    ~config:(Tezos_base_unix.Internal_event_unix.make_with_defaults ~log_cfg ())
    ()

let () =
  Random.self_init () ;
  ignore
    Tezos_clic.(
      setup_formatter
        ~isatty:(Unix.isatty Unix.stdout)
        Format.std_formatter
        Details) ;
  ignore
    Tezos_clic.(
      setup_formatter
        ~isatty:(Unix.isatty Unix.stderr)
        Format.err_formatter
        Details) ;
  Lwt.Exception_filter.(set handle_all_except_runtime) ;
  Tezos_base_unix.Event_loop.main_run ~process_name:"etherlink" (fun () ->
      Lwt_exit.wrap_and_exit
        (let open Lwt_syntax in
         let* () = init_log () in
         dispatch (argv ())))
  |> handle_error
