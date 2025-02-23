(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let install_finalizer_rpc server_public_finalizer =
  let open Lwt_syntax in
  Lwt_exit.register_clean_up_callback ~loc:__LOC__ @@ fun exit_status ->
  let* () = Events.shutdown_node ~exit_status in
  let* () = server_public_finalizer () in
  Misc.unwrap_error_monad @@ fun () -> Tx_pool.shutdown ()

let set_metrics_level (ctxt : Evm_ro_context.t) =
  let open Lwt_result_syntax in
  let+ candidate = Evm_store.(use ctxt.store Context_hashes.find_latest) in
  match candidate with
  | Some (Qty latest, _) -> Metrics.set_level ~level:latest
  | None -> ()

let set_metrics_confirmed_levels (ctxt : Evm_ro_context.t) =
  let open Lwt_result_syntax in
  let+ candidate = Evm_store.(use ctxt.store L1_l2_levels_relationships.find) in
  match candidate with
  | Some {l1_level; finalized = Qty finalized; _} ->
      Metrics.set_confirmed_level ~level:finalized ;
      Metrics.set_l1_level ~level:l1_level
  | None -> ()

let main ~data_dir ~evm_node_endpoint ?evm_node_private_endpoint
    ~(config : Configuration.t) () =
  let open Lwt_result_syntax in
  let* time_between_blocks =
    Evm_services.get_time_between_blocks
      ~fallback:(Time_between_blocks 10.)
      ~evm_node_endpoint
      ()
  in
  let* ctxt = Evm_ro_context.load ~data_dir config in
  let* () = Evm_ro_context.preload_known_kernels ctxt in

  let* legacy_block_storage =
    Evm_store.(use ctxt.store Block_storage_mode.legacy)
  in
  if not legacy_block_storage then
    Block_storage_setup.enable ~keep_alive:config.keep_alive ctxt.store ;

  let rpc_backend = Evm_ro_context.ro_backend ctxt config ~evm_node_endpoint in
  let* () =
    Tx_pool.start
      {
        backend = rpc_backend;
        smart_rollup_address =
          Tezos_crypto.Hashed.Smart_rollup_address.to_b58check
            ctxt.smart_rollup_address;
        mode =
          (match evm_node_private_endpoint with
          | Some base ->
              Forward
                {
                  injector =
                    (fun tx_object raw_tx ->
                      Injector.inject_transaction
                        ~keep_alive:config.keep_alive
                        ~base
                        ~tx_object
                        ~raw_tx);
                }
          | None -> Relay);
        tx_timeout_limit = config.tx_pool_timeout_limit;
        tx_pool_addr_limit = Int64.to_int config.tx_pool_addr_limit;
        tx_pool_tx_per_addr_limit =
          Int64.to_int config.tx_pool_tx_per_addr_limit;
        max_number_of_chunks = None;
      }
  in

  let* () = set_metrics_level ctxt in
  let* () = set_metrics_confirmed_levels ctxt in

  Metrics.init
    ~mode:"rpc"
    ~tx_pool_size_info:Tx_pool.size_info
    ~smart_rollup_address:ctxt.smart_rollup_address ;

  let* server_public_finalizer =
    Rpc_server.start_public_server
      ~delegate_health_check_to:evm_node_endpoint
      ~evm_services:
        Evm_ro_context.(evm_services_methods ctxt time_between_blocks)
      ~data_dir
      Stateless
      config
      (rpc_backend, ctxt.smart_rollup_address)
  in

  let (_ : Lwt_exit.clean_up_callback_id) =
    install_finalizer_rpc server_public_finalizer
  in

  let* next_blueprint_number = Evm_ro_context.next_blueprint_number ctxt in

  Blueprints_follower.start
    ~time_between_blocks
    ~evm_node_endpoint
    ~next_blueprint_number
  @@ fun (Qty number) blueprint ->
  let* () =
    when_ (Option.is_some blueprint.kernel_upgrade) @@ fun () ->
    Evm_ro_context.preload_kernel_from_level ctxt (Qty number)
  in
  Broadcast.notify @@ Broadcast.Blueprint blueprint ;
  Metrics.set_level ~level:number ;
  let* () = set_metrics_confirmed_levels ctxt in
  return `Continue
