(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Trilitech <contact@trili.tech>               *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Bin_evm_node
    Invocation:   cd etherlink/bin_node/test/ ;
                  dune exec ./test_transaction_object.exe
    Subject:      Tests for [Transaction_object.decode], in particular that the
                  admission path rejects transactions whose integer fields use a
                  non-canonical RLP encoding (a leading zero byte). Such a
                  transaction would be accepted by the public RPC but rejected by
                  the kernel during blueprint application, aborting the whole
                  blueprint and taking the sequencer down. *)

module Transaction_object = Evm_node_lib_dev.Transaction_object
module Ethereum_types = Evm_node_lib_dev_encoding.Ethereum_types
module Rlp = Evm_node_lib_dev_encoding.Rlp

let raw_of_hex h = Ethereum_types.(hex_to_bytes (hex_of_string h))

let error_to_string trace = Format.asprintf "%a" pp_print_trace trace

(* [contains] is not in the stdlib we link, so scan for the substring by hand. *)
let contains haystack needle =
  let n = String.length needle and m = String.length haystack in
  let rec loop i =
    if i + n > m then false
    else if String.sub haystack i n = needle then true
    else loop (i + 1)
  in
  loop 0

let is_non_canonical_error trace =
  contains (error_to_string trace) "non-canonical"

let is_length_error trace = contains (error_to_string trace) "length"

(* A canonical, real signed EIP-2930 (type 0x01) transaction taken from the
   [validate.ml] tezt suite. It must keep decoding successfully: the canonical
   check must not reject well-formed transactions. *)
let canonical_type_01 =
  "0x01f8678205dc80843b9aca008261a8945d66ec78664f4a0b0929a41270316a6cd4d8bd4b8080c080a063db2a9f77795acaf9fa62465addfd5d8e77bf564868c6762e16f3ae0c084d20a02a57aaaff92cfdde10dfa1016127ff2a94df0bf620615b50e660e76e6044a844"

(* The exact malformed EIP-1559 transaction from the vulnerability report: a
   validly signed transaction whose nonce is RLP-encoded as [0x00 0x01] instead
   of the canonical [0x01]. Before the fix this decoded successfully (the
   signature is valid) and crashed the sequencer once embedded in a blueprint. *)
let poc_padded_nonce =
  "0x02f86f820539820001843b9aca00843b9aca00830aae609400000000000000000000000000000000deadbeef8080c080a0a94cad834359f7e80b74d821e85a543d0f441d258e466e2512b366673b7d89dba01446c31eb78daccaf82d39ea3f09100dd3fcef2af96794be10214830d19513cf"

let test_canonical_accepted () =
  match Transaction_object.decode (raw_of_hex canonical_type_01) with
  | Ok _ -> ()
  | Error trace ->
      Test.fail
        "canonical EIP-2930 transaction should decode, got error: %s"
        (error_to_string trace)

let test_poc_rejected () =
  match Transaction_object.decode (raw_of_hex poc_padded_nonce) with
  | Ok _ ->
      Test.fail
        "PoC transaction with a non-canonical (padded) nonce must be rejected"
  | Error trace ->
      if not (is_non_canonical_error trace) then
        Test.fail
          "PoC transaction rejected for the wrong reason: %s"
          (error_to_string trace)

(* Canonical field values shared by the constructed cases below. *)
let chain_id = Bytes.of_string "\x05\x39"

let nonce = Bytes.of_string "\x01"

let fee = Bytes.of_string "\x3b\x9a\xca\x00"

let gas = Bytes.of_string "\x0a\xae\x60"

let value = Bytes.of_string "\x2a"

let y_parity = Bytes.of_string "\x01"

(* [to_] and [dummy_sig] are shared dummies. [to_] is a valid 20-byte address
   (an H160, as the kernel requires); the canonical integer check runs before
   signature recovery, so a padded integer field is rejected regardless of the
   (in)validity of the signature. *)
let to_ = Bytes.of_string (String.make 12 '\x00' ^ "deadbeef")

let dummy_sig = Bytes.of_string "\x01"

let padded b = Bytes.cat (Bytes.of_string "\x00") b

(* Builders producing a raw transaction of each type from raw big-endian field
   bytes, so a single field can be swapped for its non-canonical (padded)
   encoding. Field order mirrors the [reconstruct_*_from_raw] decoders in
   [Transaction_object]. *)

let eip1559_raw ~chain_id ~nonce ~max_priority ~max_fee ~gas ~value =
  let payload =
    Rlp.List
      [
        Value chain_id;
        Value nonce;
        Value max_priority;
        Value max_fee;
        Value gas;
        Value to_;
        Value value;
        Value Bytes.empty (* input *);
        List [] (* access list *);
        Value Bytes.empty (* v *);
        Value dummy_sig (* r *);
        Value dummy_sig (* s *);
      ]
  in
  Printf.sprintf "\x02%s" (Bytes.to_string (Rlp.encode payload))

let eip2930_raw ~chain_id ~nonce ~gas_price ~gas ~value =
  let payload =
    Rlp.List
      [
        Value chain_id;
        Value nonce;
        Value gas_price;
        Value gas;
        Value to_;
        Value value;
        Value Bytes.empty (* input *);
        List [] (* access list *);
        Value Bytes.empty (* v *);
        Value dummy_sig (* r *);
        Value dummy_sig (* s *);
      ]
  in
  Printf.sprintf "\x01%s" (Bytes.to_string (Rlp.encode payload))

(* A single authorization tuple, as decoded by [decode_authorization_list]. *)
let auth_item ~chain_id ~nonce ~y_parity ~r ~s =
  Rlp.List
    [
      Value chain_id;
      Value to_ (* address *);
      Value nonce;
      Value y_parity;
      Value r;
      Value s;
    ]

let canonical_auth =
  auth_item ~chain_id ~nonce ~y_parity ~r:dummy_sig ~s:dummy_sig

let eip7702_raw ~chain_id ~nonce ~max_priority ~max_fee ~gas ~value ~auth =
  let payload =
    Rlp.List
      [
        Value chain_id;
        Value nonce;
        Value max_priority;
        Value max_fee;
        Value gas;
        Value to_;
        Value value;
        Value Bytes.empty (* input *);
        List [] (* access list *);
        auth (* authorization list *);
        Value Bytes.empty (* v *);
        Value dummy_sig (* r *);
        Value dummy_sig (* s *);
      ]
  in
  "\x04" ^ Bytes.to_string (Rlp.encode payload)

(* Legacy transactions carry no type-byte prefix. *)
let legacy_raw ~nonce ~gas_price ~gas ~value =
  let payload =
    Rlp.List
      [
        Value nonce;
        Value gas_price;
        Value gas;
        Value to_;
        Value value;
        Value Bytes.empty (* input *);
        Value dummy_sig (* v *);
        Value dummy_sig (* r *);
        Value dummy_sig (* s *);
      ]
  in
  Bytes.to_string (Rlp.encode payload)

(* Sanity: a canonical build of each type must not itself trip the canonical
   check. It may still fail later (e.g. at signature recovery, because the
   signature is a dummy); we only assert it is not flagged as non-canonical.
   This guards the constructed negatives against passing for the wrong reason. *)
let check_canonical_not_rejected_as_non_canonical name raw =
  match Transaction_object.decode raw with
  | Ok _ ->
      (* A transaction succeeds decoding is not rejected *)
      ()
  | Error trace ->
      if is_non_canonical_error trace then
        Test.fail
          "canonical constructed %s wrongly flagged as non-canonical: %s"
          name
          (error_to_string trace)

let test_constructed_canonical_passes_check () =
  check_canonical_not_rejected_as_non_canonical
    "eip1559"
    (eip1559_raw ~chain_id ~nonce ~max_priority:fee ~max_fee:fee ~gas ~value) ;
  check_canonical_not_rejected_as_non_canonical
    "eip2930"
    (eip2930_raw ~chain_id ~nonce ~gas_price:fee ~gas ~value) ;
  check_canonical_not_rejected_as_non_canonical
    "legacy"
    (legacy_raw ~nonce ~gas_price:fee ~gas ~value) ;
  check_canonical_not_rejected_as_non_canonical
    "eip7702"
    (eip7702_raw
       ~chain_id
       ~nonce
       ~max_priority:fee
       ~max_fee:fee
       ~gas
       ~value
       ~auth:(Rlp.List [canonical_auth]))

let check_field_rejected name raw =
  match Transaction_object.decode raw with
  | Ok _ -> Test.fail "non-canonical %s must be rejected at admission" name
  | Error trace ->
      if not (is_non_canonical_error trace) then
        Test.fail
          "non-canonical %s rejected for the wrong reason: %s"
          name
          (error_to_string trace)

let check_length_rejected name raw =
  match Transaction_object.decode raw with
  | Ok _ -> Test.fail "wrong-length %s must be rejected at admission" name
  | Error trace ->
      if not (is_length_error trace) then
        Test.fail
          "wrong-length %s rejected for the wrong reason: %s"
          name
          (error_to_string trace)

let check_rejected name raw =
  match Transaction_object.decode raw with
  | Ok _ -> Test.fail "%s must be rejected at admission" name
  | Error _ -> ()

let test_padded_fields_eip1559 () =
  check_field_rejected
    "eip1559 chain_id"
    (eip1559_raw
       ~chain_id:(padded chain_id)
       ~nonce
       ~max_priority:fee
       ~max_fee:fee
       ~gas
       ~value) ;
  check_field_rejected
    "eip1559 nonce"
    (eip1559_raw
       ~chain_id
       ~nonce:(padded nonce)
       ~max_priority:fee
       ~max_fee:fee
       ~gas
       ~value) ;
  check_field_rejected
    "eip1559 max_priority_fee_per_gas"
    (eip1559_raw
       ~chain_id
       ~nonce
       ~max_priority:(padded fee)
       ~max_fee:fee
       ~gas
       ~value) ;
  check_field_rejected
    "eip1559 max_fee_per_gas"
    (eip1559_raw
       ~chain_id
       ~nonce
       ~max_priority:fee
       ~max_fee:(padded fee)
       ~gas
       ~value) ;
  check_field_rejected
    "eip1559 gas_limit"
    (eip1559_raw
       ~chain_id
       ~nonce
       ~max_priority:fee
       ~max_fee:fee
       ~gas:(padded gas)
       ~value) ;
  check_field_rejected
    "eip1559 value"
    (eip1559_raw
       ~chain_id
       ~nonce
       ~max_priority:fee
       ~max_fee:fee
       ~gas
       ~value:(padded value))

let test_padded_fields_eip2930 () =
  check_field_rejected
    "eip2930 chain_id"
    (eip2930_raw ~chain_id:(padded chain_id) ~nonce ~gas_price:fee ~gas ~value) ;
  check_field_rejected
    "eip2930 nonce"
    (eip2930_raw ~chain_id ~nonce:(padded nonce) ~gas_price:fee ~gas ~value) ;
  check_field_rejected
    "eip2930 gas_price"
    (eip2930_raw ~chain_id ~nonce ~gas_price:(padded fee) ~gas ~value) ;
  check_field_rejected
    "eip2930 gas_limit"
    (eip2930_raw ~chain_id ~nonce ~gas_price:fee ~gas:(padded gas) ~value) ;
  check_field_rejected
    "eip2930 value"
    (eip2930_raw ~chain_id ~nonce ~gas_price:fee ~gas ~value:(padded value))

let test_padded_fields_legacy () =
  check_field_rejected
    "legacy nonce"
    (legacy_raw ~nonce:(padded nonce) ~gas_price:fee ~gas ~value) ;
  check_field_rejected
    "legacy gas_price"
    (legacy_raw ~nonce ~gas_price:(padded fee) ~gas ~value) ;
  check_field_rejected
    "legacy gas_limit"
    (legacy_raw ~nonce ~gas_price:fee ~gas:(padded gas) ~value) ;
  check_field_rejected
    "legacy value"
    (legacy_raw ~nonce ~gas_price:fee ~gas ~value:(padded value))

let test_padded_fields_eip7702 () =
  let auth = Rlp.List [canonical_auth] in
  (* Top-level integer fields. *)
  check_field_rejected
    "eip7702 chain_id"
    (eip7702_raw
       ~chain_id:(padded chain_id)
       ~nonce
       ~max_priority:fee
       ~max_fee:fee
       ~gas
       ~value
       ~auth) ;
  check_field_rejected
    "eip7702 nonce"
    (eip7702_raw
       ~chain_id
       ~nonce:(padded nonce)
       ~max_priority:fee
       ~max_fee:fee
       ~gas
       ~value
       ~auth) ;
  check_field_rejected
    "eip7702 max_priority_fee_per_gas"
    (eip7702_raw
       ~chain_id
       ~nonce
       ~max_priority:(padded fee)
       ~max_fee:fee
       ~gas
       ~value
       ~auth) ;
  check_field_rejected
    "eip7702 max_fee_per_gas"
    (eip7702_raw
       ~chain_id
       ~nonce
       ~max_priority:fee
       ~max_fee:(padded fee)
       ~gas
       ~value
       ~auth) ;
  check_field_rejected
    "eip7702 gas_limit"
    (eip7702_raw
       ~chain_id
       ~nonce
       ~max_priority:fee
       ~max_fee:fee
       ~gas:(padded gas)
       ~value
       ~auth) ;
  check_field_rejected
    "eip7702 value"
    (eip7702_raw
       ~chain_id
       ~nonce
       ~max_priority:fee
       ~max_fee:fee
       ~gas
       ~value:(padded value)
       ~auth) ;
  (* Authorization-list integer fields (the [decode_authorization_list] path). *)
  let with_auth item =
    eip7702_raw
      ~chain_id
      ~nonce
      ~max_priority:fee
      ~max_fee:fee
      ~gas
      ~value
      ~auth:(Rlp.List [item])
  in
  check_field_rejected
    "eip7702 auth chain_id"
    (with_auth
       (auth_item
          ~chain_id:(padded chain_id)
          ~nonce
          ~y_parity
          ~r:dummy_sig
          ~s:dummy_sig)) ;
  check_field_rejected
    "eip7702 auth nonce"
    (with_auth
       (auth_item
          ~chain_id
          ~nonce:(padded nonce)
          ~y_parity
          ~r:dummy_sig
          ~s:dummy_sig)) ;
  check_rejected
    "eip7702 auth y_parity"
    (with_auth
       (auth_item
          ~chain_id
          ~nonce
          ~y_parity:(padded y_parity)
          ~r:dummy_sig
          ~s:dummy_sig)) ;
  check_field_rejected
    "eip7702 auth r"
    (with_auth
       (auth_item ~chain_id ~nonce ~y_parity ~r:(padded dummy_sig) ~s:dummy_sig)) ;
  check_field_rejected
    "eip7702 auth s"
    (with_auth
       (auth_item ~chain_id ~nonce ~y_parity ~r:dummy_sig ~s:(padded dummy_sig)))

(* Address fields ([to], access-list entries, authorization-list entries) must
   be exactly 20 bytes and storage keys exactly 32 bytes: the kernel decodes
   them as [H160]/[H256], so a wrong length passes admission only to abort the
   blueprint. These are rejected by length, not by the canonical-integer check. *)
let short_address = Bytes.of_string (String.make 19 '\x00')

let long_address = Bytes.of_string (String.make 21 '\x00')

let short_storage_key = Bytes.of_string (String.make 31 '\x00')

let access_list_with ~address ~storage_keys =
  Rlp.List
    [
      Rlp.List
        [Value address; Rlp.List (List.map (fun k -> Rlp.Value k) storage_keys)];
    ]

(* Explicit builders for the length tests: unlike the shared builders above,
   they take the address / access-list fields directly. *)
let eip1559_with ~to_ ~access_list =
  let payload =
    Rlp.List
      [
        Value chain_id;
        Value nonce;
        Value fee (* max_priority *);
        Value fee (* max_fee *);
        Value gas;
        Value to_;
        Value value;
        Value Bytes.empty (* input *);
        access_list;
        Value Bytes.empty (* v *);
        Value dummy_sig (* r *);
        Value dummy_sig (* s *);
      ]
  in
  Printf.sprintf "\x02%s" (Bytes.to_string (Rlp.encode payload))

let eip7702_with_auth_address ~address =
  let auth =
    Rlp.List
      [
        Rlp.List
          [
            Value chain_id;
            Value address;
            Value nonce;
            Value y_parity;
            Value dummy_sig (* r *);
            Value dummy_sig (* s *);
          ];
      ]
  in
  eip7702_raw ~chain_id ~nonce ~max_priority:fee ~max_fee:fee ~gas ~value ~auth

let test_wrong_length_fields () =
  (* Top-level [to]. *)
  check_length_rejected
    "eip1559 to (19 bytes)"
    (eip1559_with ~to_:short_address ~access_list:(Rlp.List [])) ;
  check_length_rejected
    "eip1559 to (21 bytes)"
    (eip1559_with ~to_:long_address ~access_list:(Rlp.List [])) ;
  (* Access-list entry address and storage key. *)
  check_length_rejected
    "eip1559 access-list address (19 bytes)"
    (eip1559_with
       ~to_
       ~access_list:(access_list_with ~address:short_address ~storage_keys:[])) ;
  check_length_rejected
    "eip1559 access-list storage key (31 bytes)"
    (eip1559_with
       ~to_
       ~access_list:
         (access_list_with ~address:to_ ~storage_keys:[short_storage_key])) ;
  (* Authorization-list entry address (EIP-7702). *)
  check_length_rejected
    "eip7702 auth address (19 bytes)"
    (eip7702_with_auth_address ~address:short_address)

let oversized_u64 = Bytes.of_string ("\001" ^ String.make 8 '\000')

let oversized_u256 = Bytes.of_string ("\001" ^ String.make 32 '\000')

let oversized_u8 = Bytes.of_string "\001\000"

let test_kernel_numeric_widths () =
  check_rejected
    "eip1559 nonce wider than u64"
    (eip1559_raw
       ~chain_id
       ~nonce:oversized_u64
       ~max_priority:fee
       ~max_fee:fee
       ~gas
       ~value) ;
  check_rejected
    "eip1559 max_fee_per_gas wider than U256"
    (eip1559_raw
       ~chain_id
       ~nonce
       ~max_priority:fee
       ~max_fee:oversized_u256
       ~gas
       ~value) ;
  check_rejected
    "legacy gas_limit wider than u64"
    (legacy_raw ~nonce ~gas_price:fee ~gas:oversized_u64 ~value) ;
  check_rejected
    "eip7702 authorization nonce wider than u64"
    (eip7702_raw
       ~chain_id
       ~nonce
       ~max_priority:fee
       ~max_fee:fee
       ~gas
       ~value
       ~auth:
         (Rlp.List
            [
              auth_item
                ~chain_id
                ~nonce:oversized_u64
                ~y_parity
                ~r:dummy_sig
                ~s:dummy_sig;
            ])) ;
  check_rejected
    "eip7702 authorization y_parity wider than u8"
    (eip7702_raw
       ~chain_id
       ~nonce
       ~max_priority:fee
       ~max_fee:fee
       ~gas
       ~value
       ~auth:
         (Rlp.List
            [
              auth_item
                ~chain_id
                ~nonce
                ~y_parity:oversized_u8
                ~r:dummy_sig
                ~s:dummy_sig;
            ]))

let is_invalid_recovery_id_error trace =
  contains (error_to_string trace) "Invalid recovery id"

let is_invalid_typed_y_parity_error trace =
  contains (error_to_string trace) "Invalid typed transaction y_parity"

let check_not_invalid_recovery_id name raw =
  match Transaction_object.decode raw with
  | Ok _ -> ()
  | Error trace ->
      if is_invalid_recovery_id_error trace then
        Test.fail
          "%s wrongly rejected as an invalid recovery id: %s"
          name
          (error_to_string trace)

let check_invalid_recovery_id name raw =
  match Transaction_object.decode raw with
  | Ok _ -> Test.fail "%s must be rejected as an invalid recovery id" name
  | Error trace ->
      if not (is_invalid_recovery_id_error trace) then
        Test.fail
          "%s rejected for the wrong reason: %s"
          name
          (error_to_string trace)

let check_invalid_typed_y_parity name raw =
  match Transaction_object.decode raw with
  | Ok _ -> Test.fail "%s must be rejected as an invalid typed y_parity" name
  | Error trace ->
      if not (is_invalid_typed_y_parity_error trace) then
        Test.fail
          "%s rejected for the wrong reason: %s"
          name
          (error_to_string trace)

let eip2930_raw_with_v v =
  let payload =
    Rlp.List
      [
        Value chain_id;
        Value nonce;
        Value fee;
        Value gas;
        Value to_;
        Value value;
        Value Bytes.empty;
        List [];
        Value v;
        Value dummy_sig;
        Value dummy_sig;
      ]
  in
  Printf.sprintf "\x01%s" (Bytes.to_string (Rlp.encode payload))

let eip1559_raw_with_v v =
  let payload =
    Rlp.List
      [
        Value chain_id;
        Value nonce;
        Value fee;
        Value fee;
        Value gas;
        Value to_;
        Value value;
        Value Bytes.empty;
        List [];
        Value v;
        Value dummy_sig;
        Value dummy_sig;
      ]
  in
  Printf.sprintf "\x02%s" (Bytes.to_string (Rlp.encode payload))

let eip7702_raw_with_v v =
  let payload =
    Rlp.List
      [
        Value chain_id;
        Value nonce;
        Value fee;
        Value fee;
        Value gas;
        Value to_;
        Value value;
        Value Bytes.empty;
        List [];
        Rlp.List [canonical_auth];
        Value v;
        Value dummy_sig;
        Value dummy_sig;
      ]
  in
  "\x04" ^ Bytes.to_string (Rlp.encode payload)

let test_typed_transaction_recovery_id () =
  let zero = Bytes.empty in
  let one = Bytes.of_string "\001" in
  let two = Bytes.of_string "\002" in
  let three = Bytes.of_string "\003" in
  let twenty_seven = Bytes.of_string "\027" in
  let twenty_eight = Bytes.of_string "\028" in
  List.iter
    (fun (tx_type, raw_with_v) ->
      check_not_invalid_recovery_id
        (Printf.sprintf "%s recovery id 0" tx_type)
        (raw_with_v zero) ;
      check_not_invalid_recovery_id
        (Printf.sprintf "%s recovery id 1" tx_type)
        (raw_with_v one) ;
      check_invalid_recovery_id
        (Printf.sprintf "%s recovery id 2" tx_type)
        (raw_with_v two) ;
      check_invalid_recovery_id
        (Printf.sprintf "%s recovery id 3" tx_type)
        (raw_with_v three) ;
      check_invalid_typed_y_parity
        (Printf.sprintf "%s y_parity 27" tx_type)
        (raw_with_v twenty_seven) ;
      check_invalid_typed_y_parity
        (Printf.sprintf "%s y_parity 28" tx_type)
        (raw_with_v twenty_eight))
    [
      ("EIP-2930", eip2930_raw_with_v);
      ("EIP-1559", eip1559_raw_with_v);
      ("EIP-7702", eip7702_raw_with_v);
    ]

let test_empty_raw_transaction () = check_rejected "empty raw transaction" ""

let tests =
  [
    ( "Transaction_object.decode",
      [
        ("canonical transaction accepted", `Quick, test_canonical_accepted);
        ("PoC padded-nonce transaction rejected", `Quick, test_poc_rejected);
        ( "constructed canonical passes the canonical check (all types)",
          `Quick,
          test_constructed_canonical_passes_check );
        ( "padded integer fields rejected (EIP-1559)",
          `Quick,
          test_padded_fields_eip1559 );
        ( "padded integer fields rejected (EIP-2930)",
          `Quick,
          test_padded_fields_eip2930 );
        ( "padded integer fields rejected (legacy)",
          `Quick,
          test_padded_fields_legacy );
        ( "padded integer fields rejected (EIP-7702, incl. authorization list)",
          `Quick,
          test_padded_fields_eip7702 );
        ( "wrong-length address and storage-key fields rejected",
          `Quick,
          test_wrong_length_fields );
        ( "kernel numeric width limits rejected",
          `Quick,
          test_kernel_numeric_widths );
        ( "typed transaction recovery id validation",
          `Quick,
          test_typed_transaction_recovery_id );
        ("empty raw transaction rejected", `Quick, test_empty_raw_transaction);
      ] );
  ]

let () = Alcotest.run ~__FILE__ "transaction_object" tests

let () = Test.run ()
