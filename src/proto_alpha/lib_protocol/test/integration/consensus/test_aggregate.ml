(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:  Protocol (aggregate)
    Invocation: dune exec src/proto_alpha/lib_protocol/test/integration/consensus/main.exe \
                  -- --file test_aggregate.ml
*)

open Protocol

(* Init genesis with 8 accounts including at least 5 BLS *)
let init_genesis_with_some_bls_accounts ?policy ?dal_enable
    ?aggregate_attestation () =
  let open Lwt_result_syntax in
  let*? random_accounts = Account.generate_accounts 3 in
  let*? bls_accounts =
    List.init ~when_negative_length:[] 5 (fun _ ->
        Account.new_account ~algo:Signature.Bls ())
  in
  let bootstrap_accounts =
    Account.make_bootstrap_accounts (random_accounts @ bls_accounts)
  in
  let* genesis =
    Block.genesis
      ?dal_enable
      ?aggregate_attestation
      ~consensus_threshold_size:0
      bootstrap_accounts
  in
  let* b = Block.bake ?policy genesis in
  return (genesis, b)

let aggregate_in_mempool_error = function
  | Validate_errors.Consensus.Aggregate_in_mempool -> true
  | _ -> false

let aggregate_disabled_error = function
  | Validate_errors.Consensus.Aggregate_disabled -> true
  | _ -> false

let aggregate_unimplemented_error = function
  | Validate_errors.Consensus.Aggregate_not_implemented -> true
  | _ -> false

let signature_invalid_error = function
  | Operation_repr.Invalid_signature -> true
  | _ -> false

let non_bls_in_aggregate = function
  | Validate_errors.Consensus.Non_bls_key_in_aggregate -> true
  | _ -> false

let find_aggregate_result receipt =
  let result_opt =
    List.find_map
      (function
        | Tezos_protocol_alpha__Protocol.Apply_results.Operation_metadata
            {
              contents =
                Single_result (Attestations_aggregate_result _ as result);
            } ->
            Some result
        | _ -> None)
      receipt
  in
  match result_opt with
  | Some res -> res
  | None -> Test.fail "No aggregate result found"

(* [check_attestations_aggregate_result ~committee result] verifies that
   [result] has the following properties:
   - [balance_update] is empty;
   - [voting_power] equals the sum of slots owned by attesters in [committee];
   - the public key hashes in [result] committee match those of [committee]. *)
let check_attestations_aggregate_result ~committee
    (result :
      Alpha_context.Kind.attestations_aggregate
      Tezos_protocol_alpha__Protocol.Apply_results.contents_result) =
  let open Lwt_result_syntax in
  match result with
  | Attestations_aggregate_result
      {balance_updates; committee = resulting_committee; consensus_power} ->
      (* Check balance updates *)
      let* () =
        match balance_updates with
        | [] -> return_unit
        | _ -> Test.fail "Unexpected non-empty balance updates list"
      in
      (* Check voting power *)
      let* () =
        let voting_power =
          List.fold_left
            (fun acc (delegate : RPC.Validators.t) ->
              List.length delegate.slots + acc)
            0
            committee
        in
        if voting_power = consensus_power then return_unit
        else
          Test.fail
            "Wrong voting power : expected %d, found %d"
            voting_power
            consensus_power
      in
      (* Check committee *)
      let committee_pkhs =
        List.map
          (fun (consensus_key : RPC.Validators.t) -> consensus_key.delegate)
          committee
      in
      let resulting_committee_pkhs =
        List.map
          (fun (attester : Alpha_context.Consensus_key.t) -> attester.delegate)
          resulting_committee
      in
      if
        List.equal
          Tezos_crypto.Signature.Public_key_hash.equal
          resulting_committee_pkhs
          committee_pkhs
      then return_unit
      else
        let pp =
          Format.(
            pp_print_list
              ~pp_sep:pp_print_cut
              Tezos_crypto.Signature.Public_key_hash.pp)
        in
        Test.fail
          "@[<v 0>Wrong commitee@,@[<v 2>expected:@,%a@]@,@[<v 2>found:@,%a@]@]"
          pp
          committee_pkhs
          pp
          resulting_committee_pkhs

(* [find_attester_with_bls_key attesters] returns the first attester with a BLS
   key, if any. *)
let find_attester_with_bls_key =
  List.find_map (fun (attester : RPC.Validators.t) ->
      match (attester.consensus_key, attester.slots) with
      | Bls _, slot :: _ -> Some (attester, slot)
      | _ -> None)

(* [find_attester_with_non_bls_key attesters] returns the first attester
   with a non-BLS key, if any. *)
let find_attester_with_non_bls_key =
  List.find_map (fun (attester : RPC.Validators.t) ->
      match (attester.consensus_key, attester.slots) with
      | (Ed25519 _ | Secp256k1 _ | P256 _), slot :: _ -> Some (attester, slot)
      | _ -> None)

let test_aggregate_feature_flag_enabled () =
  let open Lwt_result_syntax in
  let* _genesis, attested_block =
    init_genesis_with_some_bls_accounts ~aggregate_attestation:true ()
  in
  Consensus_helpers.test_consensus_operation_all_modes_different_outcomes
    ~loc:__LOC__
    ~attested_block
    ~mempool_error:aggregate_in_mempool_error
    Aggregate

let test_aggregate_feature_flag_disabled () =
  let open Lwt_result_syntax in
  let* _genesis, attested_block =
    init_genesis_with_some_bls_accounts ~aggregate_attestation:false ()
  in
  Consensus_helpers.test_consensus_operation_all_modes_different_outcomes
    ~loc:__LOC__
    ~attested_block
    ~application_error:aggregate_disabled_error
    ~construction_error:aggregate_disabled_error
    ~mempool_error:aggregate_in_mempool_error
    Aggregate

let test_aggregate_attestation_with_a_single_bls_attestation () =
  let open Lwt_result_syntax in
  let* _genesis, block =
    init_genesis_with_some_bls_accounts ~aggregate_attestation:true ()
  in
  let* attesters = Context.get_attesters (B block) in
  (* Find an attester with a BLS consensus key. *)
  let attester, slot =
    WithExceptions.Option.get
      ~loc:__LOC__
      (find_attester_with_bls_key attesters)
  in
  let* attestation =
    Op.raw_attestation ~delegate:attester.RPC.Validators.delegate ~slot block
  in
  let operation =
    WithExceptions.Option.get ~loc:__LOC__ (Op.aggregate [attestation])
  in
  let* _, (_, receipt) = Block.bake_with_metadata ~operation block in
  let result = find_aggregate_result receipt in
  check_attestations_aggregate_result ~committee:[attester] result

let test_aggregate_attestation_with_multiple_bls_attestations () =
  let open Lwt_result_syntax in
  let* _genesis, block =
    init_genesis_with_some_bls_accounts ~aggregate_attestation:true ()
  in
  let* attesters = Context.get_attesters (B block) in
  (* Filter delegates with BLS keys that have at least one slot *)
  let* bls_delegates_with_slots =
    List.filter_map_es
      (fun (attester : RPC.Validators.t) ->
        match (attester.consensus_key, attester.slots) with
        | Bls _, slot :: _ -> return_some (attester, slot)
        | _ -> return_none)
      attesters
  in
  let* attestations =
    List.map_es
      (fun (delegate, slot) ->
        Op.raw_attestation
          ~delegate:delegate.RPC.Validators.delegate
          ~slot
          block)
      bls_delegates_with_slots
  in
  let aggregation =
    WithExceptions.Option.get ~loc:__LOC__ (Op.aggregate attestations)
  in
  let* _, (_, receipt) =
    Block.bake_with_metadata ~operation:aggregation block
  in
  let result = find_aggregate_result receipt in
  let delegates = List.map fst bls_delegates_with_slots in
  check_attestations_aggregate_result ~committee:delegates result

let test_aggregate_attestation_invalid_signature () =
  let open Lwt_result_syntax in
  let* _genesis, block =
    init_genesis_with_some_bls_accounts ~aggregate_attestation:true ()
  in
  let* attesters = Context.get_attesters (B block) in
  (* Find an attester with a BLS consensus key. *)
  let attester, _ =
    WithExceptions.Option.get
      ~loc:__LOC__
      (find_attester_with_bls_key attesters)
  in
  (* Craft an aggregate with a single attestation signed by this delegate *)
  let* aggregate =
    Op.attestations_aggregate ~committee:[attester.consensus_key] block
  in
  (* Swap the signature for Signature.Bls.zero *)
  match aggregate.protocol_data with
  | Operation_data {contents; _} ->
      let aggregate_with_incorrect_signature =
        {
          aggregate with
          protocol_data =
            Operation_data {contents; signature = Some (Bls Signature.Bls.zero)};
        }
      in
      (* Bake a block containing this operation and expect an error *)
      let*! res =
        Block.bake ~operation:aggregate_with_incorrect_signature block
      in
      Assert.proto_error ~loc:__LOC__ res signature_invalid_error

let test_aggregate_attestation_non_bls_delegate () =
  let open Lwt_result_syntax in
  let* _genesis, block =
    init_genesis_with_some_bls_accounts ~aggregate_attestation:true ()
  in
  let* attesters = Context.get_attesters (B block) in
  (* Find an attester with a non-BLS consensus key. *)
  let attester, slot =
    WithExceptions.Option.get
      ~loc:__LOC__
      (find_attester_with_non_bls_key attesters)
  in
  (* Craft an attestation for this attester to retreive a signature and a
     triplet {level, round, block_payload_hash} *)
  let* {shell; protocol_data = {contents; signature}} =
    Op.raw_attestation ~delegate:attester.RPC.Validators.delegate ~slot block
  in
  match contents with
  | Single (Attestation {consensus_content; _}) ->
      let {level; round; block_payload_hash; _} :
          Alpha_context.consensus_content =
        consensus_content
      in
      (* Craft an aggregate including the attester slot and signature *)
      let consensus_content : Alpha_context.consensus_aggregate_content =
        {level; round; block_payload_hash}
      in
      let contents : _ Alpha_context.contents_list =
        Single (Attestations_aggregate {consensus_content; committee = [slot]})
      in
      let aggregate : operation =
        {shell; protocol_data = Operation_data {contents; signature}}
      in
      (* Bake a block containing this aggregate and expect an error *)
      let*! res = Block.bake ~operation:aggregate block in
      Assert.proto_error ~loc:__LOC__ res non_bls_in_aggregate

let tests =
  [
    Tztest.tztest
      "test_aggregate_feature_flag_enabled"
      `Quick
      test_aggregate_feature_flag_enabled;
    Tztest.tztest
      "test_aggregate_feature_flag_disabled"
      `Quick
      test_aggregate_feature_flag_disabled;
    Tztest.tztest
      "test_aggregate_attestation_with_a_single_bls_attestation"
      `Quick
      test_aggregate_attestation_with_a_single_bls_attestation;
    Tztest.tztest
      "test_aggregate_attestation_with_multiple_bls_attestations"
      `Quick
      test_aggregate_attestation_with_multiple_bls_attestations;
    Tztest.tztest
      "test_aggregate_attestation_invalid_signature"
      `Quick
      test_aggregate_attestation_invalid_signature;
    Tztest.tztest
      "test_aggregate_attestation_non_bls_delegate"
      `Quick
      test_aggregate_attestation_non_bls_delegate;
  ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("aggregate", tests)]
  |> Lwt_main.run
