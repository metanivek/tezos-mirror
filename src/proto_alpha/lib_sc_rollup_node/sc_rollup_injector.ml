(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 Functori, <contact@functori.com>                       *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Protocol
open Alpha_context
open Injector_common
open Injector_sigs
module Block_cache =
  Aches_lwt.Lache.Make_result
    (Aches.Rache.Transfer (Aches.Rache.LRU) (Block_hash))

let injector_operation_to_manager :
    L1_operation.t -> Protocol.Alpha_context.packed_manager_operation = function
  | Add_messages {messages} -> Manager (Sc_rollup_add_messages {messages})
  | Cement {rollup; commitment = _} ->
      let rollup = Sc_rollup_proto_types.Address.of_octez rollup in
      Manager (Sc_rollup_cement {rollup})
  | Publish {rollup; commitment} ->
      let rollup = Sc_rollup_proto_types.Address.of_octez rollup in
      let commitment = Sc_rollup_proto_types.Commitment.of_octez commitment in
      Manager (Sc_rollup_publish {rollup; commitment})
  | Refute {rollup; opponent; refutation} ->
      let rollup = Sc_rollup_proto_types.Address.of_octez rollup in
      let refutation =
        Sc_rollup_proto_types.Game.refutation_of_octez refutation
      in
      let opponent = Signature.Of_V_latest.get_public_key_hash_exn opponent in
      Manager (Sc_rollup_refute {rollup; opponent; refutation})
  | Timeout {rollup; stakers} ->
      let rollup = Sc_rollup_proto_types.Address.of_octez rollup in
      let stakers = Sc_rollup_proto_types.Game.index_of_octez stakers in
      Manager (Sc_rollup_timeout {rollup; stakers})
  | Recover_bond {rollup; staker} ->
      let rollup = Sc_rollup_proto_types.Address.of_octez rollup in
      let staker = Signature.Of_V_latest.get_public_key_hash_exn staker in
      Manager (Sc_rollup_recover_bond {sc_rollup = rollup; staker})
  | Execute_outbox_message {rollup; cemented_commitment; output_proof} ->
      let rollup = Sc_rollup_proto_types.Address.of_octez rollup in
      let cemented_commitment =
        Sc_rollup_proto_types.Commitment_hash.of_octez cemented_commitment
      in
      Manager
        (Sc_rollup_execute_outbox_message
           {rollup; cemented_commitment; output_proof})
  | Publish_dal_commitment {slot_index; commitment; commitment_proof} ->
      let open Sc_rollup_proto_types.Dal in
      (* FIXME: https://gitlab.com/tezos/tezos/-/issues/7319

         commitment and commitment_proof should be versionned in
         Sc_rollup_proto_types.Dal *)

      (* Below we use number_of_slots = slot_index + 1, because we don't have
         access to number_of_slots parameters.  We could add it to
         [Publish_dal_commitment], but then, we'll not be able to set it
         correctly in function {!injector_operation_of_manager} below. *)
      let number_of_slots = slot_index + 1 in
      Manager
        (Dal_publish_commitment
           {
             slot_index = Slot_index.of_octez ~number_of_slots slot_index;
             commitment;
             commitment_proof;
           })

let injector_operation_of_manager :
    type kind.
    kind Protocol.Alpha_context.manager_operation -> L1_operation.t option =
  function
  | Sc_rollup_add_messages {messages} -> Some (Add_messages {messages})
  | Sc_rollup_cement {rollup} ->
      let rollup = Sc_rollup_proto_types.Address.to_octez rollup in
      let commitment = Octez_smart_rollup.Commitment.Hash.zero in
      (* Just for printing *)
      Some (Cement {rollup; commitment})
  | Sc_rollup_publish {rollup; commitment} ->
      let rollup = Sc_rollup_proto_types.Address.to_octez rollup in
      let commitment = Sc_rollup_proto_types.Commitment.to_octez commitment in
      Some (Publish {rollup; commitment})
  | Sc_rollup_refute {rollup; opponent; refutation} ->
      let rollup = Sc_rollup_proto_types.Address.to_octez rollup in
      let refutation =
        Sc_rollup_proto_types.Game.refutation_to_octez refutation
      in
      let opponent = Tezos_crypto.Signature.Of_V2.public_key_hash opponent in
      Some (Refute {rollup; opponent; refutation})
  | Sc_rollup_timeout {rollup; stakers} ->
      let rollup = Sc_rollup_proto_types.Address.to_octez rollup in
      let stakers = Sc_rollup_proto_types.Game.index_to_octez stakers in
      Some (Timeout {rollup; stakers})
  | Sc_rollup_execute_outbox_message {rollup; cemented_commitment; output_proof}
    ->
      let rollup = Sc_rollup_proto_types.Address.to_octez rollup in
      let cemented_commitment =
        Sc_rollup_proto_types.Commitment_hash.to_octez cemented_commitment
      in
      Some (Execute_outbox_message {rollup; cemented_commitment; output_proof})
  | Dal_publish_commitment {slot_index; commitment; commitment_proof} ->
      Some
        (Publish_dal_commitment
           {
             slot_index =
               Sc_rollup_proto_types.Dal.Slot_index.to_octez slot_index;
             commitment;
             commitment_proof;
           })
  | _ -> None

module Proto_client = struct
  open Protocol_client_context

  type operation = L1_operation.t

  type state = Injector.state

  type unsigned_operation =
    Tezos_base.Operation.shell_header * packed_contents_list

  let max_operation_data_length = Constants.max_operation_data_length

  let manager_pass = Operation_repr.manager_pass

  let manager_operation_size (Manager operation) =
    let contents =
      Manager_operation
        {
          source = Signature.Public_key_hash.zero;
          operation;
          fee = Tez.zero;
          counter = Manager_counter.Internal_for_tests.of_int 0;
          gas_limit = Gas.Arith.zero;
          storage_limit = Z.zero;
        }
    in
    Data_encoding.Binary.length Operation.contents_encoding (Contents contents)

  let operation_size op =
    manager_operation_size (injector_operation_to_manager op)

  (* The operation size overhead is an upper bound (in practice) of the overhead
     that will be added to a manager operation. To compute it we can use any
     manager operation (here a revelation), add an overhead with upper bounds as
     values (for the fees, limits, counters, etc.) and compare the encoded
     operations with respect to their size.
     NOTE: This information is only used to pre-select operations from the
     injector queue as a candidate batch. *)
  let operation_size_overhead =
    let dummy_operation =
      Reveal
        {
          public_key =
            Signature.Public_key.of_b58check_exn
              "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav";
          proof = None;
        }
    in
    let dummy_contents =
      Manager_operation
        {
          source = Signature.Public_key_hash.zero;
          operation = dummy_operation;
          fee = Tez.of_mutez_exn 3_000_000L;
          counter = Manager_counter.Internal_for_tests.of_int 500_000;
          gas_limit = Gas.Arith.integral_of_int_exn 500_000;
          storage_limit = Z.of_int 500_000;
        }
    in
    let dummy_size =
      Data_encoding.Binary.length
        Operation.contents_encoding
        (Contents dummy_contents)
    in
    dummy_size - manager_operation_size (Manager dummy_operation)

  let manager_operation_result_status (type kind)
      (op_result : kind Apply_results.manager_operation_result) :
      operation_status =
    match op_result with
    | Applied _ -> Successful
    | Backtracked (_, None) -> Unsuccessful Backtracked
    | Skipped _ -> Unsuccessful Skipped
    | Backtracked (_, Some err)
    (* Backtracked because internal operation failed *)
    | Failed (_, err) ->
        Unsuccessful (Failed (Environment.wrap_tztrace err))

  let operation_result_status (type kind)
      (op_result : kind Apply_results.contents_result) : operation_status =
    match op_result with
    | Preattestation_result _ -> Successful
    | Attestation_result _ -> Successful
    | Preattestations_aggregate_result _ -> Successful
    | Attestations_aggregate_result _ -> Successful
    | Seed_nonce_revelation_result _ -> Successful
    | Vdf_revelation_result _ -> Successful
    | Double_consensus_operation_evidence_result _ -> Successful
    | Double_baking_evidence_result _ -> Successful
    | Dal_entrapment_evidence_result _ -> Successful
    | Activate_account_result _ -> Successful
    | Proposals_result -> Successful
    | Ballot_result -> Successful
    | Drain_delegate_result _ -> Successful
    | Manager_operation_result {operation_result; _} ->
        manager_operation_result_status operation_result

  let operation_contents_status (type kind)
      (contents : kind Apply_results.contents_result_list) ~index :
      operation_status tzresult =
    let rec rec_status :
        type kind. int -> kind Apply_results.contents_result_list -> _ =
     fun n -> function
      | Apply_results.Single_result _ when n <> 0 ->
          error_with "No operation with index %d" index
      | Single_result result -> Ok (operation_result_status result)
      | Cons_result (result, _rest) when n = 0 ->
          Ok (operation_result_status result)
      | Cons_result (_result, rest) -> rec_status (n - 1) rest
    in
    rec_status index contents

  let operation_status_of_receipt (operation : Protocol.operation_receipt)
      ~index : operation_status tzresult =
    match (operation : _) with
    | No_operation_metadata ->
        error_with "Cannot find operation status because metadata is missing"
    | Operation_metadata {contents} -> operation_contents_status contents ~index

  (* TODO: https://gitlab.com/tezos/tezos/-/issues/6339 *)
  (* Don't make multiple calls to [operations_in_pass] RPC *)
  let get_block_operations =
    let ops_cache = Block_cache.create 32 in
    fun cctxt block_hash ->
      Block_cache.bind_or_put
        ops_cache
        block_hash
        (fun block_hash ->
          let open Lwt_result_syntax in
          let+ operations =
            Alpha_block_services.Operations.operations_in_pass
              cctxt
              ~chain:cctxt#chain
              ~block:(`Hash (block_hash, 0))
              ~metadata:`Always
              manager_pass
          in
          List.fold_left
            (fun acc (op : Alpha_block_services.operation) ->
              Operation_hash.Map.add op.hash op acc)
            Operation_hash.Map.empty
            operations)
        Lwt.return

  let operation_status {Injector.cctxt; _} block_hash operation_hash ~index =
    let open Lwt_result_syntax in
    let* operations = get_block_operations cctxt block_hash in
    match Operation_hash.Map.find_opt operation_hash operations with
    | None -> return_none
    | Some operation -> (
        match operation.receipt with
        | Empty ->
            failwith "Cannot find operation status because metadata is empty"
        | Too_large ->
            failwith
              "Cannot find operation status because metadata is too large"
        | Receipt receipt ->
            let*? status = operation_status_of_receipt receipt ~index in
            return_some status)

  let dummy_sk_uri =
    WithExceptions.Result.get_ok ~loc:__LOC__
    @@ Tezos_signer_backends.Unencrypted.make_sk
    @@ Tezos_crypto.Signature.Secret_key.of_b58check_exn
         "edsk3UqeiQWXX7NFEY1wUs6J1t2ez5aQ3hEWdqX5Jr5edZiGLW8nZr"

  let simulate_operations cctxt ~force ~source ~src_pk ~successor_level
      ~fee_parameter ?safety_guard operations =
    let open Lwt_result_syntax in
    let fee_parameter : Injection.fee_parameter =
      {
        minimal_fees = Tez.of_mutez_exn fee_parameter.minimal_fees.mutez;
        minimal_nanotez_per_byte = fee_parameter.minimal_nanotez_per_byte;
        minimal_nanotez_per_gas_unit =
          fee_parameter.minimal_nanotez_per_gas_unit;
        force_low_fee = fee_parameter.force_low_fee;
        fee_cap = Tez.of_mutez_exn fee_parameter.fee_cap.mutez;
        burn_cap = Tez.of_mutez_exn fee_parameter.burn_cap.mutez;
      }
    in
    let open Annotated_manager_operation in
    let annotated_operations =
      List.map
        (fun operation ->
          let (Manager operation) = injector_operation_to_manager operation in
          Annotated_manager_operation
            (Injection.prepare_manager_operation
               ~fee:Limit.unknown
               ~gas_limit:Limit.unknown
               ~storage_limit:Limit.unknown
               operation))
        operations
    in
    let (Manager_list annot_op) =
      Annotated_manager_operation.manager_of_list annotated_operations
    in
    let cctxt =
      new Protocol_client_context.wrap_full (cctxt :> Client_context.full)
    in
    let safety_guard = Option.map Gas.Arith.integral_of_int_exn safety_guard in
    let*! simulation_result =
      let*? source = Signature.Of_V_latest.get_public_key_hash source in
      let*? src_pk = Signature.Of_V_latest.get_public_key src_pk in
      Injection.inject_manager_operation
        cctxt
        ~simulation:true (* Only simulation here *)
        ~force
        ~chain:cctxt#chain
        ~block:(`Head 0)
        ~source
        ~src_pk
        ~src_sk:dummy_sk_uri
          (* Use dummy secret key as it is not used by simulation *)
        ~successor_level
        ~fee:Limit.unknown
        ~gas_limit:Limit.unknown
        ~storage_limit:Limit.unknown
        ?safety_guard
        ~fee_parameter
        annot_op
    in
    match simulation_result with
    | Error trace ->
        let exceeds_quota =
          TzTrace.fold
            (fun exceeds -> function
              | Environment.Ecoproto_error
                  (Gas.Block_quota_exceeded | Gas.Operation_quota_exceeded) ->
                  true
              | _ -> exceeds)
            false
            trace
        in
        fail (if exceeds_quota then `Exceeds_quotas trace else `TzError trace)
    | Ok (_oph, packed_op, _contents, results) ->
        let nb_ops = List.length operations in
        let results = Apply_results.to_list (Contents_result_list results) in
        (* packed_op can have reveal operations added automatically. *)
        let start_index = List.length results - nb_ops in
        (* remove extra reveal operations *)
        let operations_statuses =
          List.fold_left_i
            (fun index_in_batch acc (Apply_results.Contents_result result) ->
              if index_in_batch < start_index then acc
              else
                {index_in_batch; status = operation_result_status result} :: acc)
            []
            results
          |> List.rev
        in
        let unsigned_operation =
          let {shell; protocol_data = Operation_data {contents; signature = _}}
              =
            packed_op
          in
          (shell, Contents_list contents)
        in
        return {operations_statuses; unsigned_operation}

  let sign_operation cctxt src_sk
      ((shell, Contents_list contents) as unsigned_op) =
    let open Lwt_result_syntax in
    let unsigned_bytes =
      Data_encoding.Binary.to_bytes_exn Operation.unsigned_encoding unsigned_op
    in
    let cctxt =
      new Protocol_client_context.wrap_full (cctxt :> Client_context.full)
    in
    let+ signature =
      Client_keys.sign
        cctxt
        ~watermark:Signature.Generic_operation
        src_sk
        unsigned_bytes
    in
    let op : packed_operation =
      {
        shell;
        protocol_data = Operation_data {contents; signature = Some signature};
      }
    in
    Data_encoding.Binary.to_bytes_exn Operation.encoding op

  let time_until_next_block
      {Injector.minimal_block_delay; delay_increment_per_round; _}
      (header : Tezos_base.Block_header.shell_header option) =
    let open Result_syntax in
    match header with
    | None -> minimal_block_delay |> Int64.to_int |> Ptime.Span.of_int_s
    | Some header ->
        let minimal_block_delay = Period.of_seconds_exn minimal_block_delay in
        let delay_increment_per_round =
          Period.of_seconds_exn delay_increment_per_round
        in
        let next_level_timestamp =
          let* durations =
            Round.Durations.create
              ~first_round_duration:minimal_block_delay
              ~delay_increment_per_round
          in
          let* predecessor_round = Fitness.round_from_raw header.fitness in
          Round.timestamp_of_round
            durations
            ~predecessor_timestamp:header.timestamp
            ~predecessor_round
            ~round:Round.zero
        in
        let next_level_timestamp =
          Result.value
            next_level_timestamp
            ~default:
              (WithExceptions.Result.get_ok
                 ~loc:__LOC__
                 Timestamp.(header.timestamp +? minimal_block_delay))
        in
        Ptime.diff
          (Time.System.of_protocol_exn next_level_timestamp)
          (Time.System.now ())

  let check_fee_parameters Injector.{fee_parameters; _} =
    let check_value operation_kind name compare to_string mempool_default value
        =
      if compare mempool_default value > 0 then
        error_with
          "Bad configuration fee_parameter.%s for %s. It must be at least %s \
           for operations of the injector to be propagated."
          name
          (Operation_kind.to_string operation_kind)
          (to_string mempool_default)
      else Ok ()
    in
    let check purpose
        {
          minimal_fees;
          minimal_nanotez_per_byte;
          minimal_nanotez_per_gas_unit;
          force_low_fee = _;
          fee_cap = _;
          burn_cap = _;
        } =
      let open Result_syntax in
      let+ () =
        check_value
          purpose
          "minimal_fees"
          Int64.compare
          Int64.to_string
          (Protocol.Alpha_context.Tez.to_mutez
             Plugin.Mempool.default_minimal_fees)
          minimal_fees.mutez
      and+ () =
        check_value
          purpose
          "minimal_nanotez_per_byte"
          Q.compare
          Q.to_string
          Plugin.Mempool.default_minimal_nanotez_per_byte
          minimal_nanotez_per_byte
      and+ () =
        check_value
          purpose
          "minimal_nanotez_per_gas_unit"
          Q.compare
          Q.to_string
          Plugin.Mempool.default_minimal_nanotez_per_gas_unit
          minimal_nanotez_per_gas_unit
      in
      ()
    in
    Operation_kind.Map.iter_e check fee_parameters

  let checks state = check_fee_parameters state

  let get_balance_mutez cctxt ?block pkh =
    let open Lwt_result_syntax in
    let block = match block with Some b -> `Hash (b, 0) | None -> `Head 0 in
    let cctxt =
      new Protocol_client_context.wrap_full (cctxt :> Client_context.full)
    in
    let*? pkh = Signature.Of_V_latest.get_public_key_hash pkh in
    let+ balance =
      Plugin.Alpha_services.Contract.balance
        cctxt
        (cctxt#chain, block)
        (Implicit pkh)
    in
    Protocol.Alpha_context.Tez.to_mutez balance
end

let () = Injector.register_proto_client Protocol.hash (module Proto_client)
