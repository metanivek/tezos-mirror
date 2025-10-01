(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

open Setup
open Benchmark_utils
open Floodgate_lib
open Evm_node_lib_dev_encoding

type env = {
  sequencer : Evm_node.t;
  rpc_node : Evm_node.t;
  infos : Network_info.t;
  gas_limit : Z.t;
  accounts : Floodgate_lib.Account.t array;
  nb_contracts : int;
}

let deposit_one {infos; gas_limit; _} ?nonce value account contract =
  let* _ =
    call infos contract gas_limit account ?nonce ~value:(Z.of_int value) [] []
  in
  unit

let deposit_gas_limit {accounts; infos; rpc_node; _} contract =
  let sender = accounts.(0) in
  let rpc_endpoint = Evm_node.endpoint rpc_node |> Uri.of_string in
  let*? gas_limit =
    Network_info.get_gas_limit
      ~rpc_endpoint
      ~base_fee_per_gas:infos.base_fee_per_gas
      ~from:(Account.address_et sender)
      ~to_:(Address (Hex contract))
      ~value:(Z.of_int 1_000_000)
      ()
  in
  Log.debug "Deposit gas limit: %a@." Z.pp_print gas_limit ;
  return gas_limit

let deposits env erc20s =
  Log.info "Depositing tokens to ERC20 contracts" ;
  let* gas_limit = deposit_gas_limit env (List.hd erc20s) in
  let env = {env with gas_limit} in
  let f () =
    Lwt_list.iter_p
      (fun account ->
        let nonce = account.Account.nonce in
        Lwt_list.iteri_p
          (fun i contract ->
            let nonce = Z.add nonce (Z.of_int i) in
            deposit_one env ~nonce 1_000_000 account contract)
          erc20s)
      (Array.to_list env.accounts)
  in
  wait_for_application env.sequencer f

let get_total_supply {infos; rpc_node; accounts; _} contract =
  let* res =
    let open Evm_node_lib_dev in
    Batch.call
      (module Rpc_encodings.Eth_call)
      ~evm_node_endpoint:(Evm_node.endpoint rpc_node |> Uri.of_string)
      ~keep_alive:true
      ( {
          from = Some (Account.address_et accounts.(0));
          to_ = Some (Ethereum_types.Address.of_string contract);
          gas = None;
          gasPrice = Some (Qty infos.base_fee_per_gas);
          value = Some (Qty Z.zero);
          data =
            Some
              ((Efunc_core.Evm.encode ~name:"totalSupply" [] [] :> string)
              |> Ethereum_types.hash_of_string);
        },
        Block_parameter Latest,
        Ethereum_types.AddressMap.empty )
  in
  match res with
  | Error e ->
      Test.fail "supply error: %a" Tezos_base.TzPervasives.pp_print_trace e
  | Ok (Ethereum_types.Hash (Ethereum_types.Hex s)) ->
      Helpers.decode_z_be (`Hex s |> Hex.to_bytes) |> Z.to_int |> return

let check_deposits env erc20s =
  let* () =
    Lwt_list.iter_p
      (fun contract ->
        let* supply = get_total_supply env contract in
        Check.((supply = Array.length env.accounts * 1_000_000) int)
          ~error_msg:(sf "Total supply for %s is %%L instead of %%R" contract) ;
        unit)
      erc20s
  in
  Log.report "Deposited tokens to ERC20 contracts" ;
  unit

let transfer_gas_limit {accounts; infos; rpc_node; _} contract =
  let sender = accounts.(0) in
  let dest = accounts.(1 mod Array.length accounts) in
  let rpc_endpoint = Evm_node.endpoint rpc_node |> Uri.of_string in
  let data =
    Efunc_core.Evm.encode
      ~name:"transfer"
      [`address; `uint 256]
      [`address (Account.address dest); `int (Z.of_int 1000)]
  in
  let data = Ethereum_types.hash_of_string (data :> string) in
  let*? gas_limit =
    Network_info.get_gas_limit
      ~rpc_endpoint
      ~base_fee_per_gas:infos.base_fee_per_gas
      ~from:(Account.address_et sender)
      ~to_:(Address (Hex contract))
      ~data
      ~value:Z.zero
      ()
  in
  return gas_limit

let account_step infos gas_limit contract ~nonce (sender : Account.t) value
    (dest : Account.t) =
  let* _ =
    call
      infos
      contract
      gas_limit
      sender
      ~nonce
      ~name:"transfer"
      [`address; `uint 256]
      [`address (Account.address dest); `int (Z.of_int value)]
  in
  unit

let sender_step {infos; gas_limit; accounts; _} erc20s iteration sender_index =
  let sender = accounts.(sender_index mod Array.length accounts) in
  let dest_index =
    (sender_index + (7 * (iteration + 1))) mod Array.length accounts
  in
  let dest = accounts.(dest_index) in
  let nonce = sender.nonce in
  Lwt_list.iteri_p
    (fun i contract ->
      let nonce = Z.add nonce (Z.of_int i) in
      account_step
        infos
        gas_limit
        contract
        ~nonce
        sender
        (iteration + i + sender_index)
        dest)
    erc20s

let step ({sequencer; accounts; _} as env) erc20s iteration =
  Log.report "Iteration %d" iteration ;
  let sender_indexes = List.init (Array.length accounts) Fun.id in
  let step_f () =
    Lwt_list.iter_p (sender_step env erc20s iteration) sender_indexes
  in
  wait_for_application sequencer step_f

let test_erc20_capacity =
  let nb_accounts = Option.value parameters.accounts ~default:100 in
  let nb_contracts = Option.value parameters.contracts ~default:10 in
  let accounts = Eth_account.accounts nb_accounts in
  let eth_bootstrap_accounts =
    Array.to_list accounts |> List.map (fun a -> a.Eth_account.address)
  in
  register_all
    ~__FILE__
    ~tags:["erc20"; "benchmark"; "capacity"; "ci_disabled"]
    ~title:"Capacity of EVM node"
    ~time_between_blocks:Nothing
    ~eth_bootstrap_accounts
    ~websockets:true
    ~use_multichain:Register_without_feature
    ~use_dal:Register_without_feature
    ~da_fee:Wei.zero
    ~minimum_base_fee_per_gas:Wei.one
    ~maximum_gas_per_transaction:(1 lsl 50 |> Int64.of_int)
    ~tx_queue:{max_lifespan = 4; max_size = 4_000; tx_per_addr_limit = 1024}
  @@ fun {sequencer; evm_version = _; _} _protocol ->
  let* accounts = floodgate_accounts sequencer accounts in
  let endpoint = Evm_node.endpoint sequencer |> Uri.of_string in
  let*? infos =
    Network_info.fetch ~rpc_endpoint:endpoint ~base_fee_factor:100.
  in
  let env =
    {
      infos;
      sequencer;
      rpc_node = sequencer;
      gas_limit = Z.zero;
      accounts;
      nb_contracts;
    }
  in
  let*? () =
    Tx_queue.start
      ~relay_endpoint:endpoint
      ~max_transaction_batch_length:(Some 300)
      ~inclusion_timeout:parameters.timeout
      ()
  in
  let follower =
    Floodgate.start_blueprint_follower
      ~relay_endpoint:endpoint
      ~rpc_endpoint:endpoint
  in
  let tx_queue = Tx_queue.beacon ~tick_interval:0.1 in
  Log.report "Deploying %d ERC20 contracts" nb_contracts ;
  let* erc20s =
    deploy_contracts
      ~rpc_node:env.rpc_node
      infos
      ~sequencer
      accounts
      `ERC20
      nb_contracts
  in
  Log.info "%d ERC20 contracts deployed" (List.length erc20s) ;
  List.iter (Log.debug "- %s") erc20s ;
  let* () = deposits env erc20s in
  let* () = check_deposits env erc20s in
  let* gas_limit = transfer_gas_limit env (List.hd erc20s) in
  Log.debug "Transfer gas limit: %a@." Z.pp_print gas_limit ;
  let env = {env with gas_limit} in
  monitor_gasometer sequencer @@ fun () ->
  let* stop_profile =
    if parameters.profiling then profile sequencer else return (fun () -> unit)
  in
  let* () =
    Lwt_list.iter_s (step env erc20s) (List.init parameters.iterations succ)
  in
  Lwt.cancel follower ;
  Lwt.cancel tx_queue ;
  let* () = Tx_queue.shutdown () in
  let* () = Evm_node.terminate sequencer in
  stop_profile ()

let register () = test_erc20_capacity [Protocol.Alpha]
