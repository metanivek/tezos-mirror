(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Bin_evm_node
    Invocation:   dune exec etherlink/bin_node/test/test_tezlink_prevalidation.exe
    Subject:      Unit tests for Tezlink_prevalidation: the
                  [parse_unvalidated] accumulator and the L2 chain-id
                  conversion.
*)
open Evm_node_lib_dev

open Evm_node_lib_dev_tezlink

let test_register =
  Test.register
    ~uses_node:false
    ~uses_client:false
    ~uses_admin_client:false
    ~__FILE__

let source_zero = Tezos_crypto.Signature.V3.Public_key_hash.zero

let z_typ = Check.equalable Z.pp_print Z.equal

let manager_content ?(source = source_zero) ~counter ~fee ~gas_limit () =
  let open Tezlink_imports.Imported_context in
  Manager_operation
    {
      source;
      fee = Tez.of_mutez_exn (Int64.of_int fee);
      counter = Manager_counter.Internal_for_tests.of_int counter;
      gas_limit = Gas.Arith.integral_exn (Z.of_int gas_limit);
      storage_limit = Z.zero;
      operation =
        Transaction
          {
            amount = Tez.zero;
            parameters = Script.unit_parameter;
            entrypoint = Entrypoint.default;
            destination = Contract.Implicit source;
          };
    }

let pack_and_encode
    (contents : _ Tezlink_imports.Imported_context.contents_list) =
  let open Tezlink_imports.Imported_context in
  let packed_op : packed_operation =
    {
      shell = {branch = Tezos_crypto.Hashed.Block_hash.zero};
      protocol_data =
        Operation_data
          {contents; signature = Some Tezos_crypto.Signature.V3.zero};
    }
  in
  (packed_op, Data_encoding.Binary.to_bytes_exn Operation.encoding packed_op)

let parse_unvalidated contents =
  let _op, raw = pack_and_encode contents in
  Tezlink_prevalidation.Internal_for_tests.parse_unvalidated raw

let expect_ok_lwt ~__LOC__ p =
  let open Lwt.Syntax in
  let* r = p in
  match r with
  | Ok x -> Lwt.return x
  | Error trace ->
      Test.fail ~__LOC__ "Unexpected error: %a" Error_monad.pp_print_trace trace

let test_parse_unvalidated_accumulates_fee_and_gas =
  test_register
    ~title:"parse_unvalidated accumulates fee and gas_limit"
    ~tags:["tezosx"; "prevalidation"; "parsing"]
  @@ fun () ->
  let open Lwt.Syntax in
  let* operation =
    expect_ok_lwt
      ~__LOC__
      (parse_unvalidated
         (Single (manager_content ~counter:1 ~fee:12345 ~gas_limit:42_000 ())))
  in
  Check.(
    (operation.length = 1) int ~__LOC__ ~error_msg:"expected length=%R, got %L") ;
  Check.(
    (operation.first_counter = Z.one)
      z_typ
      ~__LOC__
      ~error_msg:"expected first_counter=%R, got %L") ;
  Check.(
    (Tezos_types.Tez.to_mutez_z operation.fee |> Z.to_int = 12345)
      int
      ~__LOC__
      ~error_msg:"expected fee=%R mutez, got %L") ;
  Check.(
    (Z.to_int operation.gas_limit = 42_000)
      int
      ~__LOC__
      ~error_msg:"expected gas_limit=%R, got %L") ;
  unit

(* A [Cons] batch must sum lengths, fees and gas_limits — the arm the
   [Single] test above cannot reach. *)
let test_parse_unvalidated_accepts_cons_batch =
  test_register
    ~title:"parse_unvalidated handles a Cons batch and sums fee/gas"
    ~tags:["tezosx"; "prevalidation"; "parsing"; "batch"]
  @@ fun () ->
  let open Lwt.Syntax in
  let* operation =
    expect_ok_lwt
      ~__LOC__
      (parse_unvalidated
         (Cons
            ( manager_content ~counter:1 ~fee:100 ~gas_limit:1_000 (),
              Single (manager_content ~counter:2 ~fee:250 ~gas_limit:3_000 ())
            )))
  in
  Check.(
    (operation.length = 2) int ~__LOC__ ~error_msg:"expected length=%R, got %L") ;
  Check.(
    (Tezos_types.Tez.to_mutez_z operation.fee |> Z.to_int = 350)
      int
      ~__LOC__
      ~error_msg:"expected fee=%R mutez (100+250), got %L") ;
  Check.(
    (Z.to_int operation.gas_limit = 4_000)
      int
      ~__LOC__
      ~error_msg:"expected gas_limit=%R (1000+3000), got %L") ;
  Check.(
    (Z.to_int operation.first_counter = 1)
      int
      ~__LOC__
      ~error_msg:"expected first_counter=%R, got %L") ;
  unit

let source_alt =
  Tezos_crypto.Signature.V3.Public_key_hash.of_b58check_exn
    "tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU"

(* Pins the documented contract that [parse_unvalidated] does NOT
   enforce source consistency across a batch: the result's [source] is
   the first content's. If a future change tightens this, update the
   .mli docstring to match. *)
let test_parse_unvalidated_does_not_check_source_consistency =
  test_register
    ~title:"parse_unvalidated does not enforce source consistency in a batch"
    ~tags:["tezosx"; "prevalidation"; "parsing"; "batch"; "contract"]
  @@ fun () ->
  let open Lwt.Syntax in
  let* operation =
    expect_ok_lwt
      ~__LOC__
      (parse_unvalidated
         (Cons
            ( manager_content
                ~source:source_zero
                ~counter:1
                ~fee:0
                ~gas_limit:1_000
                (),
              Single
                (manager_content
                   ~source:source_alt
                   ~counter:2
                   ~fee:0
                   ~gas_limit:1_000
                   ()) )))
  in
  let pkh_typ =
    Check.equalable
      Tezos_crypto.Signature.V3.Public_key_hash.pp
      Tezos_crypto.Signature.V3.Public_key_hash.equal
  in
  Check.(
    (operation.source = source_zero)
      pkh_typ
      ~__LOC__
      ~error_msg:
        "parse_unvalidated must surface only the first content's source (%R), \
         got %L") ;
  Check.(
    (operation.length = 2) int ~__LOC__ ~error_msg:"expected length=%R, got %L") ;
  unit

let test_parse_unvalidated_rejects_malformed_bytes =
  test_register
    ~title:"parse_unvalidated rejects malformed bytes"
    ~tags:["tezosx"; "prevalidation"; "parsing"; "negative"]
  @@ fun () ->
  let open Lwt.Syntax in
  let raw = Bytes.of_string "\x00\x01\x02\x03not a real operation" in
  let* r = Tezlink_prevalidation.Internal_for_tests.parse_unvalidated raw in
  (match r with
  | Ok _ -> Test.fail "Expected parse_unvalidated to reject malformed bytes"
  | Error _ -> ()) ;
  unit

let test_parse_unvalidated_rejects_non_manager_operation =
  test_register
    ~title:"parse_unvalidated rejects a non-manager operation"
    ~tags:["tezosx"; "prevalidation"; "parsing"; "negative"]
  @@ fun () ->
  let open Lwt.Syntax in
  let open Tezlink_imports.Imported_context in
  let* r =
    parse_unvalidated (Single (Failing_noop "not a manager operation"))
  in
  (match r with
  | Ok _ ->
      Test.fail "Expected parse_unvalidated to reject a non-manager operation"
  | Error _ -> ()) ;
  unit

(* The kernel derives L2 chain ids from raw chain-id bytes with
   [u32::to_le_bytes()] (etherlink/kernel_latest/kernel/src/chains.rs);
   this pins the matching OCaml decoding. The bytes have non-trivial
   high-order bytes so a big-endian misread (0xefbeadde) is
   distinguishable from the expected LE value (0xdeadbeef). *)
let test_chain_id_little_endian =
  test_register
    ~title:"l2_chain_id_from_protocol_chain_id uses little-endian"
    ~tags:["tezosx"; "chain_id"; "endianness"]
  @@ fun () ->
  let bytes_le = Bytes.of_string "\xef\xbe\xad\xde" in
  let chain_id = Tezos_crypto.Hashed.Chain_id.of_bytes_exn bytes_le in
  let (Evm_node_lib_dev_encoding.L2_types.Chain_id z) =
    Tezos_types.l2_chain_id_from_protocol_chain_id chain_id
  in
  Check.(
    (z = Z.of_string "0xdeadbeef")
      z_typ
      ~__LOC__
      ~error_msg:"chain_id little-endian conversion: expected %R, got %L") ;
  unit

let () =
  test_parse_unvalidated_accumulates_fee_and_gas ;
  test_parse_unvalidated_accepts_cons_batch ;
  test_parse_unvalidated_does_not_check_source_consistency ;
  test_parse_unvalidated_rejects_malformed_bytes ;
  test_parse_unvalidated_rejects_non_manager_operation ;
  test_chain_id_little_endian

let () = Test.run ()
