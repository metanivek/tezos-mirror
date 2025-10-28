(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type parameters = {
  signer : Signer.map;
  maximum_number_of_chunks : int;
  tx_container : Services_backend_sig.ex_tx_container;
  sequencer_sunset_sec : int64;
  preconfirmation_stream_enabled : bool;
}

(* The size of a delayed transaction is overapproximated to the maximum size
   of an inbox message, as chunks are not supported in the delayed bridge. *)
let maximum_delayed_transaction_size = 4096

(*
   The legacy transactions are as follows:
  -----------------------------
  | Nonce    | Up to 32 bytes |
  -----------------------------
  | GasPrice | Up to 32 bytes |
  -----------------------------
  | GasLimit | Up to 32 bytes |
  -----------------------------
  | To       | 20 bytes addr  |
  -----------------------------
  | Value    | Up to 32 bytes |
  -----------------------------
  | Data     | 0 - unlimited  |
  -----------------------------
  | V        | 1 (usually)    |
  -----------------------------
  | R        | 32 bytes       |
  -----------------------------
  | S        | 32 bytes       |
  -----------------------------

   where `up to` start at 0, and encoded as the empty byte for the 0 value
   according to RLP specification.
*)
let minimum_ethereum_transaction_size =
  Rlp.(
    List
      [
        Value Bytes.empty;
        Value Bytes.empty;
        Value Bytes.empty;
        Value (Bytes.make 20 '\000');
        Value Bytes.empty;
        Value Bytes.empty;
        Value Bytes.empty;
        Value (Bytes.make 32 '\000');
        Value (Bytes.make 32 '\000');
      ]
    |> encode |> Bytes.length)

module Types = struct
  type nonrec parameters = parameters

  type state = {
    signer : Signer.map;
    maximum_number_of_chunks : int;
    tx_container : Services_backend_sig.ex_tx_container;
    sequencer_sunset_sec : int64;
    mutable sunset : bool;
    preconfirmation_stream_enabled : bool;
    mutable validated_txns :
      (string * Tx_queue_types.transaction_object_t) list;
    mutable validation_state : int * Prevalidator.validation_state;
    mutable next_block_timestamp : Time.System.t option;
  }
end

module Name = struct
  type t = unit

  let encoding = Data_encoding.unit

  let base = ["evm_node_worker"; "block"; "producer"]

  let pp _ _ = ()

  let equal () () = true
end

module Request = struct
  type ('a, 'b) t =
    | Produce_block :
        (bool * Time.Protocol.t * bool)
        -> ([`Block_produced of int | `No_block], tztrace) t
    | Preconfirm_transactions :
        (string * Tx_queue_types.transaction_object_t) list
        -> (unit, tztrace) t

  let name : type a b. (a, b) t -> string = function
    | Produce_block _ -> "Produce_block"
    | Preconfirm_transactions _ -> "Preconfirm_transactions"

  type view = View : _ t -> view

  let view (req : _ t) = View req

  let transaction_object_t_encoding =
    let open Data_encoding in
    let open Tx_queue_types in
    union
      [
        case
          Json_only
          ~title:"EVM"
          Transaction_object.encoding
          (function Evm txn_object -> Some txn_object | _ -> None)
          (fun txn_object -> Evm txn_object);
        case
          Json_only
          ~title:"Michelson"
          Tezos_types.Operation.encoding
          (function Michelson op -> Some op | _ -> None)
          (fun op -> Michelson op);
      ]

  let encoding =
    let open Data_encoding in
    union
      [
        case
          Json_only
          ~title:"Produce_block"
          (obj4
             (req "request" (constant "produce_block"))
             (req "with_delayed_transactions" bool)
             (req "timestamp" Time.Protocol.encoding)
             (req "force" bool))
          (function
            | View (Produce_block (with_delayed_transactions, timestamp, force))
              ->
                Some ((), with_delayed_transactions, timestamp, force)
            | _ -> None)
          (fun ((), with_delayed_transactions, timestamp, force) ->
            View (Produce_block (with_delayed_transactions, timestamp, force)));
        case
          Json_only
          ~title:"Preconfirm_transactions"
          (obj2
             (req "request" (constant "preconfirm_transactions"))
             (req
                "transactions"
                (list (tup2 string transaction_object_t_encoding))))
          (function
            | View (Preconfirm_transactions transactions) ->
                Some ((), transactions)
            | _ -> None)
          (fun ((), transactions) ->
            View (Preconfirm_transactions transactions));
      ]

  let pp _ppf (View _) = ()
end

module Worker = Octez_telemetry.Worker.MakeSingle (Name) (Request) (Types)

type worker = Worker.infinite Worker.queue Worker.t

let take_delayed_transactions evm_state maximum_number_of_chunks =
  let open Lwt_result_syntax in
  let maximum_cumulative_size =
    Sequencer_blueprint.maximum_usable_space_in_blueprint
      maximum_number_of_chunks
  in
  let maximum_delayed_transactions =
    maximum_cumulative_size / maximum_delayed_transaction_size
  in
  let*! delayed_transactions = Evm_state.delayed_inbox_hashes evm_state in
  let delayed_transactions =
    List.take_n maximum_delayed_transactions delayed_transactions
  in
  let remaining_cumulative_size =
    maximum_cumulative_size - (List.length delayed_transactions * 4096)
  in
  return (delayed_transactions, remaining_cumulative_size)

let produce_block_with_transactions ~signer ~timestamp ~transactions_and_objects
    ~delayed_hashes ~hash_of_tx_object head_info =
  let open Lwt_result_syntax in
  let transactions, tx_hashes =
    List.to_seq transactions_and_objects
    |> Seq.map (fun (raw, obj) -> (raw, hash_of_tx_object obj))
    |> Seq.split
    |> fun (l, r) -> (List.of_seq l, List.of_seq r)
  in
  Misc.with_timing
    (Blueprint_events.blueprint_production
       head_info.Evm_context.next_blueprint_number)
  @@ fun () ->
  let chunks =
    Sequencer_blueprint.make_blueprint_chunks
      ~number:head_info.next_blueprint_number
      {
        parent_hash = head_info.current_block_hash;
        delayed_transactions = delayed_hashes;
        transactions;
        timestamp;
      }
  in
  (* Resolve the content of delayed transactions. *)
  let* delayed_transactions =
    List.map_es
      (fun delayed_hash ->
        Evm_state.get_delayed_inbox_item head_info.evm_state delayed_hash)
      delayed_hashes
  in
  let* blueprint_chunks, payload, confirmed_txs =
    Evm_context.apply_chunks ~signer timestamp chunks delayed_transactions
  in
  let (Qty number) = head_info.next_blueprint_number in
  let* () =
    Blueprints_publisher.publish
      number
      (Blueprints_publisher_types.Request.Blueprint
         {chunks = blueprint_chunks; inbox_payload = payload})
  in
  let*! () =
    List.iter_p
      (fun hash -> Block_producer_events.transaction_selected ~hash)
      (tx_hashes @ delayed_hashes)
  in
  return confirmed_txs

type _ transaction_object_list =
  | Evm_tx_objects :
      (string * Transaction_object.t) list
      -> L2_types.evm_chain_family transaction_object_list
  | Michelson_tx_objects :
      (string * Tezos_types.Operation.t) list
      -> L2_types.michelson_chain_family transaction_object_list

let produce_block_with_transactions (type f)
    ~(transactions_and_objects : f transaction_object_list option)
    ~(tx_container : f Services_backend_sig.tx_container) =
  match tx_container with
  | Evm_tx_container (module Tx_container) ->
      let transactions_and_objects =
        match transactions_and_objects with
        | None -> []
        | Some (Evm_tx_objects l) -> l
      in
      produce_block_with_transactions
        ~transactions_and_objects
        ~hash_of_tx_object:Transaction_object.hash
  | Michelson_tx_container (module Tx_container) ->
      let transactions_and_objects =
        match transactions_and_objects with
        | None -> []
        | Some (Michelson_tx_objects l) -> l
      in
      produce_block_with_transactions
        ~transactions_and_objects
        ~hash_of_tx_object:Tx_queue_types.Tezlink_operation.hash_of_tx_object

let validate_tx ~maximum_cumulative_size (current_size, validation_state) raw_tx
    (tx_object : Transaction_object.t) =
  let open Lwt_result_syntax in
  let new_size = current_size + String.length raw_tx in
  if new_size > maximum_cumulative_size then return `Stop
  else
    let* validation_state_res =
      Prevalidator.validate_balance_gas_nonce_with_validation_state
        validation_state
        tx_object
    in
    match validation_state_res with
    | Ok validation_state -> return (`Keep (new_size, validation_state))
    | Error msg ->
        let hash = Transaction_object.hash tx_object in
        let*! () = Block_producer_events.transaction_rejected hash msg in
        return `Drop

let pop_valid_tx (type f) ~(tx_container : f Services_backend_sig.tx_container)
    (head_info : Evm_context.head) ~maximum_cumulative_size :
    f transaction_object_list tzresult Lwt.t =
  let open Lwt_result_syntax in
  (* Skip validation if chain_family is Michelson. *)
  match tx_container with
  | Michelson_tx_container (module Tx_container) ->
      let initial_validation_state = () in
      let* l =
        Tx_container.pop_transactions
          ~maximum_cumulative_size
          ~validate_tx:(fun () _ _ -> return (`Keep ()))
          ~initial_validation_state
      in
      return (Michelson_tx_objects l)
  | Evm_tx_container (module Tx_container) ->
      (* Low key optimization to avoid even checking the txpool if there is not
         enough space for the smallest transaction. *)
      if maximum_cumulative_size <= minimum_ethereum_transaction_size then
        return (Evm_tx_objects [])
      else
        let read = Evm_state.read head_info.evm_state in
        let* minimum_base_fee_per_gas =
          Etherlink_durable_storage.minimum_base_fee_per_gas read
        in
        let* base_fee_per_gas =
          Etherlink_durable_storage.base_fee_per_gas read
        in
        let* maximum_gas_limit =
          Etherlink_durable_storage.maximum_gas_per_transaction read
        in
        let* da_fee_per_byte = Etherlink_durable_storage.da_fee_per_byte read in
        let config =
          Prevalidator.
            {
              minimum_base_fee_per_gas = Qty minimum_base_fee_per_gas;
              base_fee_per_gas;
              maximum_gas_limit;
              da_fee_per_byte;
              next_nonce =
                (fun addr -> Etherlink_durable_storage.nonce read addr);
              balance =
                (fun addr -> Etherlink_durable_storage.balance read addr);
            }
        in
        let initial_validation_state =
          ( 0,
            Prevalidator.
              {
                config;
                addr_balance = String.Map.empty;
                addr_nonce = String.Map.empty;
              } )
        in
        let* l =
          Tx_container.pop_transactions
            ~maximum_cumulative_size
            ~validate_tx:(validate_tx ~maximum_cumulative_size)
            ~initial_validation_state
        in
        return (Evm_tx_objects l)

(** Produces a block if we find at least one valid transaction in the transaction
    pool or if [force] is true. *)
let produce_block_if_needed (type f) ~signer ~force ~timestamp ~delayed_hashes
    ~remaining_cumulative_size
    ~(tx_container : f Services_backend_sig.tx_container) head_info =
  let open Lwt_result_syntax in
  let* transactions_and_objects =
    pop_valid_tx
      ~tx_container
      head_info
      ~maximum_cumulative_size:remaining_cumulative_size
  in
  let n_txs =
    match transactions_and_objects with
    | Evm_tx_objects l -> List.length l
    | Michelson_tx_objects l -> List.length l
  in
  let n = n_txs + List.length delayed_hashes in
  if force || n > 0 then
    let* confirmed_txs =
      produce_block_with_transactions
        ~signer
        ~timestamp
        ~transactions_and_objects:(Some transactions_and_objects)
        ~delayed_hashes
        ~tx_container
        head_info
    in
    let (module Tx_container) =
      Services_backend_sig.tx_container_module tx_container
    in
    let* () =
      Tx_container.confirm_transactions
        ~clear_pending_queue_after:true
        ~confirmed_txs
    in
    return (`Block_produced n)
  else return `No_block

let head_info_and_delayed_transactions ~with_delayed_transactions evm_state
    maximum_number_of_chunks =
  let open Lwt_result_syntax in
  (* We need to first fetch the delayed transactions then requests the head info.
     If the order is swapped, we might face a race condition where the delayed
     transactions are fetched from state more recent than head info. *)
  let* delayed_hashes, remaining_cumulative_size =
    if with_delayed_transactions then
      take_delayed_transactions evm_state maximum_number_of_chunks
    else
      return
        ( [],
          Sequencer_blueprint.maximum_usable_space_in_blueprint
            maximum_number_of_chunks )
  in
  return (delayed_hashes, remaining_cumulative_size)

let init_michelson_validation_state () =
  let open Lwt_result_syntax in
  let config =
    Prevalidator.
      {
        minimum_base_fee_per_gas = Qty Z.zero;
        base_fee_per_gas = Qty Z.zero;
        maximum_gas_limit = Qty Z.zero;
        da_fee_per_byte = Qty Z.zero;
        next_nonce = (fun _addr -> return None);
        balance = (fun _addr -> return (Ethereum_types.Qty Z.zero));
      }
  in
  return
    ( 0,
      Prevalidator.
        {config; addr_balance = String.Map.empty; addr_nonce = String.Map.empty}
    )

let init_evm_validation_state () =
  let open Lwt_result_syntax in
  let*! head_info = Evm_context.head_info () in
  let read = Evm_state.read head_info.evm_state in
  let* minimum_base_fee_per_gas =
    Etherlink_durable_storage.minimum_base_fee_per_gas read
  in
  let* base_fee_per_gas = Etherlink_durable_storage.base_fee_per_gas read in
  let* maximum_gas_limit =
    Etherlink_durable_storage.maximum_gas_per_transaction read
  in
  let* da_fee_per_byte = Etherlink_durable_storage.da_fee_per_byte read in
  let config =
    Prevalidator.
      {
        minimum_base_fee_per_gas = Qty minimum_base_fee_per_gas;
        base_fee_per_gas;
        maximum_gas_limit;
        da_fee_per_byte;
        next_nonce = (fun addr -> Etherlink_durable_storage.nonce read addr);
        balance = (fun addr -> Etherlink_durable_storage.balance read addr);
      }
  in
  return
    ( 0,
      Prevalidator.
        {config; addr_balance = String.Map.empty; addr_nonce = String.Map.empty}
    )

let init_validation_state ~(tx_container : Services_backend_sig.ex_tx_container)
    =
  match tx_container with
  | Ex_tx_container tx_container -> (
      match tx_container with
      | Evm_tx_container _ -> init_evm_validation_state ()
      | Michelson_tx_container _ -> init_michelson_validation_state ())

let clear_preconfirmation_data ~(state : Types.state) =
  let open Lwt_result_syntax in
  state.validated_txns <- [] ;
  let* validation_state =
    init_validation_state ~tx_container:state.tx_container
  in
  state.validation_state <- validation_state ;
  return_unit

(* [now] is the timestamp of the previously created block.
   We compute [next] as the maximum between:
   - [now + 500 ms], ensuring a minimal delay between blocks, and
   - [now'], the actual end time of the previous block. *)
let compute_next_block_timestamp ~now =
  let now' = Ptime_clock.now () in

  let t_500 =
    Ptime.Span.of_float_s 0.5 |> WithExceptions.Option.get ~loc:__LOC__
  in
  let now_plus =
    Ptime.add_span now t_500 |> WithExceptions.Option.get ~loc:__LOC__
  in
  Time.System.(max now_plus now')

(* Required while the preconfirmation system and previous block production co-exist *)
let choose_block_timestamp preconfirmation_stream_enabled next_block_timestamp
    external_timestamp =
  match (preconfirmation_stream_enabled, next_block_timestamp) with
  | false, _ -> external_timestamp
  | true, Some next -> Time.System.to_protocol next
  | true, None -> Misc.now ()

let produce_block (state : Types.state) ~force ~(timestamp : Time.Protocol.t)
    ~with_delayed_transactions =
  let open Lwt_result_syntax in
  match state.tx_container with
  | Ex_tx_container tx_container -> (
      let (module Tx_container) =
        Services_backend_sig.tx_container_module tx_container
      in
      (*
        now and timestamp serve distinct but complementary purposes:
        - now: taken at the start of the request, used to predict the next timestamp
          and keep it aligned with real time.
        - timestamp: derived from the previously computed `next_block_timestamp`,
          ensuring consistency between preconfirmation and block creation.
      *)
      let now = Ptime_clock.now () in
      let timestamp =
        choose_block_timestamp
          state.preconfirmation_stream_enabled
          state.next_block_timestamp
          timestamp
      in
      let*! head_info = Evm_context.head_info () in
      let* () =
        when_ (not state.sunset) @@ fun () ->
        match head_info.pending_sequencer_upgrade with
        | Some Evm_events.Sequencer_upgrade.{timestamp = upgrade_timestamp; _}
          when Time.Protocol.(
                 add timestamp state.sequencer_sunset_sec >= upgrade_timestamp
                 && timestamp < upgrade_timestamp) ->
            (* We stop producing blocks ahead of the upgrade *)
            let*! () = Block_producer_events.sunset () in
            state.sunset <- true ;
            Tx_container.lock_transactions ()
        | _ -> return_unit
      in
      let* is_locked = Tx_container.is_locked () in
      if is_locked then
        let*! () = Block_producer_events.production_locked () in
        return `No_block
      else
        let* delayed_hashes, remaining_cumulative_size =
          head_info_and_delayed_transactions
            ~with_delayed_transactions
            head_info.evm_state
            state.maximum_number_of_chunks
        in
        let is_going_to_upgrade_kernel =
          match head_info.pending_upgrade with
          | Some Evm_events.Upgrade.{hash = _; timestamp = upgrade_timestamp} ->
              timestamp >= upgrade_timestamp
          | None -> false
        in
        let signer = state.signer in
        if is_going_to_upgrade_kernel then (
          let* hashes =
            produce_block_with_transactions
              ~signer
              ~timestamp
              ~transactions_and_objects:None
              ~delayed_hashes:[]
              ~tx_container
              head_info
          in
          state.next_block_timestamp <- Some (compute_next_block_timestamp ~now) ;
          (* (Seq.length hashes) is always zero, this is only to be "future" proof *)
          return (`Block_produced (Seq.length hashes)))
        else
          let* result =
            produce_block_if_needed
              ~signer
              ~timestamp
              ~force
              ~delayed_hashes
              ~remaining_cumulative_size
              ~tx_container
              head_info
          in
          state.next_block_timestamp <- Some (compute_next_block_timestamp ~now) ;
          match result with
          | `Block_produced _ ->
              let* () =
                when_ state.preconfirmation_stream_enabled (fun () ->
                    clear_preconfirmation_data ~state)
              in
              return result
          | `No_block -> return result)

let preconfirm_transactions ~(state : Types.state) ~transactions ~timestamp =
  let open Lwt_result_syntax in
  let maximum_cumulative_size =
    Sequencer_blueprint.maximum_usable_space_in_blueprint
      state.maximum_number_of_chunks
  in
  let validate (validation_state, rev_txns) ((raw, tx_object) as entry) =
    match tx_object with
    | Tx_queue_types.Evm tx_object -> (
        let+ res =
          validate_tx ~maximum_cumulative_size validation_state raw tx_object
        in
        match res with
        | `Drop -> (validation_state, rev_txns)
        | `Keep latest_validation_state ->
            if Int.equal (List.length state.validated_txns) 0 then
              Broadcast.notify_new_block_timestamp
                (Time.System.to_protocol timestamp) ;
            Broadcast.notify_preconfirmation tx_object ;
            (latest_validation_state, entry :: rev_txns)
        | `Stop -> (validation_state, rev_txns))
    | Tx_queue_types.Michelson _ ->
        (* Ignore tezlink operations for now *)
        return (validation_state, rev_txns)
  in
  let* validation_state, rev_validated_txns =
    List.fold_left_es validate (state.validation_state, []) transactions
  in
  state.validated_txns <- state.validated_txns @ List.rev rev_validated_txns ;
  state.validation_state <- validation_state ;
  return_unit

module Handlers = struct
  type self = worker

  let on_request : type r request_error.
      worker -> (r, request_error) Request.t -> (r, request_error) result Lwt.t
      =
   fun w request ->
    let state = Worker.state w in
    match request with
    | Request.Produce_block (with_delayed_transactions, timestamp, force) ->
        protect @@ fun () ->
        produce_block state ~force ~timestamp ~with_delayed_transactions
    | Request.Preconfirm_transactions transactions -> (
        protect @@ fun () ->
        (* If we are before the first created block and block producer
          is not aware of its future timestamp, preconfirmation are disabled *)
        match state.next_block_timestamp with
        | Some timestamp ->
            preconfirm_transactions ~state ~transactions ~timestamp
        | None -> Lwt_result_syntax.return_unit)

  type launch_error = error trace

  let on_launch _w ()
      ({
         signer;
         maximum_number_of_chunks;
         tx_container;
         sequencer_sunset_sec;
         preconfirmation_stream_enabled;
       } :
        Types.parameters) =
    let open Lwt_result_syntax in
    let* validation_state = init_validation_state ~tx_container in
    return
      Types.
        {
          sunset = false;
          signer;
          maximum_number_of_chunks;
          tx_container;
          sequencer_sunset_sec;
          preconfirmation_stream_enabled;
          validation_state;
          validated_txns = [];
          next_block_timestamp = None;
        }

  let on_error (type a b) _w _st (_r : (a, b) Request.t) (_errs : b) :
      [`Continue | `Shutdown] tzresult Lwt.t =
    Lwt_result_syntax.return `Continue

  let on_completion _ _ _ _ = Lwt.return_unit

  let on_no_request _ = Lwt.return_unit

  let on_close _ = Lwt.return_unit
end

let table = Worker.create_table Queue

let worker_promise, worker_waker = Lwt.task ()

type error += No_block_producer

type error += Block_producer_terminated

let worker =
  lazy
    (match Lwt.state worker_promise with
    | Lwt.Return worker -> Ok worker
    | Lwt.Fail e -> Error (TzTrace.make @@ error_of_exn e)
    | Lwt.Sleep -> Error (TzTrace.make No_block_producer))

let handle_request_error rq =
  let open Lwt_syntax in
  let* rq in
  match rq with
  | Ok res -> return_ok res
  | Error (Worker.Request_error errs) -> Lwt.return_error errs
  | Error (Closed None) -> Lwt.return_error [Block_producer_terminated]
  | Error (Closed (Some errs)) -> Lwt.return_error errs
  | Error (Any exn) -> Lwt.return_error [Exn exn]

let start parameters =
  let open Lwt_result_syntax in
  let*! () = Block_producer_events.started () in
  let+ worker = Worker.launch table () parameters (module Handlers) in
  Lwt.wakeup worker_waker worker

let shutdown () =
  let open Lwt_syntax in
  let w = Lazy.force worker in
  match w with
  | Error _ -> Lwt.return_unit
  | Ok w ->
      let* () = Block_producer_events.shutdown () in
      Worker.shutdown w

let produce_block ~with_delayed_transactions ~force ~timestamp =
  let open Lwt_result_syntax in
  let*? worker = Lazy.force worker in
  Worker.Queue.push_request_and_wait
    worker
    (Request.Produce_block (with_delayed_transactions, timestamp, force))
  |> handle_request_error

let preconfirm_transactions ~transactions =
  let open Lwt_result_syntax in
  let*? worker = Lazy.force worker in
  Worker.Queue.push_request_and_wait
    worker
    (Request.Preconfirm_transactions transactions)
  |> handle_request_error

module Internal_for_tests = struct
  let produce_block ~with_delayed_transactions =
    produce_block ~with_delayed_transactions
end

let produce_block = produce_block ~with_delayed_transactions:true
