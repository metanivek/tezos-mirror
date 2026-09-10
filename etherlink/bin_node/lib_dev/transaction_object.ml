(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(* Copyright (c) 2026 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

open Ethereum_types

type access = Ethereum_types.access = {
  address : address;
  storage_keys : hex list;
}

let access_encoding = Ethereum_types.access_encoding

type authorization_item = Ethereum_types.authorization_item = {
  chain_id : quantity;
  address : address;
  nonce : quantity;
  y_parity : quantity;
  r : quantity;
  s : quantity;
}

let authorization_item_encoding = Ethereum_types.authorization_item_encoding

module EIP_2930 = struct
  type t = {
    chain_id : quantity;
    block_hash : block_hash option;
    block_number : quantity option;
    from : address;
    gas : quantity;
    gas_price : quantity;
    hash : hash;
    input : hex;
    nonce : quantity;
    to_ : address option;
    transaction_index : quantity option;
    value : quantity;
    access_list : access list;
    v : quantity;
    r : quantity;
    s : quantity;
  }

  let encoding =
    let open Data_encoding in
    conv
      (function
        | {
            chain_id;
            block_hash;
            block_number;
            from;
            gas;
            gas_price;
            hash;
            input;
            nonce;
            to_;
            transaction_index;
            value;
            access_list;
            v;
            r;
            s;
          } ->
            ( ( chain_id,
                block_hash,
                block_number,
                from,
                gas,
                gas_price,
                hash,
                input,
                nonce,
                to_ ),
              (transaction_index, value, access_list, v, r, s) ))
      (fun ( ( chain_id,
               block_hash,
               block_number,
               from,
               gas,
               gas_price,
               hash,
               input,
               nonce,
               to_ ),
             (transaction_index, value, access_list, v, r, s) )
         ->
        {
          chain_id;
          block_hash;
          block_number;
          from;
          gas;
          gas_price;
          hash;
          input;
          nonce;
          to_;
          transaction_index;
          value;
          access_list;
          v;
          r;
          s;
        })
      (merge_objs
         (obj10
            (req "chainId" quantity_encoding)
            (req "blockHash" (option block_hash_encoding))
            (req "blockNumber" (option quantity_encoding))
            (req "from" address_encoding)
            (req "gas" quantity_encoding)
            (req "gasPrice" quantity_encoding)
            (req "hash" hash_encoding)
            (req "input" hex_encoding)
            (req "nonce" quantity_encoding)
            (req "to" (option address_encoding)))
         (obj6
            (req "transactionIndex" (option quantity_encoding))
            (req "value" quantity_encoding)
            (req "accessList" (list access_encoding))
            (req "v" quantity_encoding)
            (req "r" quantity_encoding)
            (req "s" quantity_encoding)))
end

module EIP_1559 = struct
  type t = {
    chain_id : quantity;
    hash : hash;
    nonce : quantity;
    block_hash : block_hash option;
    block_number : quantity option;
    transaction_index : quantity option;
    from : address;
    to_ : address option;
    value : quantity;
    gas : quantity;
    max_fee_per_gas : quantity;
    max_priority_fee_per_gas : quantity;
    access_list : access list;
    input : hex;
    v : quantity;
    r : quantity;
    s : quantity;
  }

  let encoding =
    let open Data_encoding in
    conv
      (fun {
             chain_id;
             hash;
             nonce;
             block_hash;
             block_number;
             transaction_index;
             from;
             to_;
             value;
             gas;
             max_fee_per_gas;
             max_priority_fee_per_gas;
             access_list;
             input;
             v;
             r;
             s;
           }
         ->
        ( ( chain_id,
            hash,
            nonce,
            block_hash,
            block_number,
            transaction_index,
            from,
            to_,
            value,
            gas ),
          ( max_fee_per_gas,
            max_priority_fee_per_gas,
            Some max_fee_per_gas
            (* Other providers set gasPrice as the effectiveGasPrice, but this
               information is only available in the receipt. *),
            access_list,
            input,
            v,
            r,
            s ) ))
      (fun ( ( chain_id,
               hash,
               nonce,
               block_hash,
               block_number,
               transaction_index,
               from,
               to_,
               value,
               gas ),
             ( max_fee_per_gas,
               max_priority_fee_per_gas,
               _gas_price,
               access_list,
               input,
               v,
               r,
               s ) )
         ->
        {
          chain_id;
          hash;
          nonce;
          block_hash;
          block_number;
          transaction_index;
          from;
          to_;
          value;
          gas;
          max_fee_per_gas;
          max_priority_fee_per_gas;
          access_list;
          input;
          v;
          r;
          s;
        })
      (merge_objs
         (obj10
            (req "chainId" quantity_encoding)
            (req "hash" hash_encoding)
            (req "nonce" quantity_encoding)
            (req "blockHash" (option block_hash_encoding))
            (req "blockNumber" (option quantity_encoding))
            (req "transactionIndex" (option quantity_encoding))
            (req "from" address_encoding)
            (req "to" (option address_encoding))
            (req "value" quantity_encoding)
            (req "gas" quantity_encoding))
         (obj8
            (req "maxFeePerGas" quantity_encoding)
            (req "maxPriorityFeePerGas" quantity_encoding)
            (opt
               "gasPrice"
               quantity_encoding
               ~description:
                 "Identical to maxFeePerGas. Purely for compatibility with \
                  hardhat although the spec specifies this should be null for \
                  EIP 1559.")
            (req "accessList" (list access_encoding))
            (req "input" hex_encoding)
            (req "v" quantity_encoding)
            (req "r" quantity_encoding)
            (req "s" quantity_encoding)))
end

module EIP_7702 = struct
  type t = {
    chain_id : quantity;
    hash : hash;
    nonce : quantity;
    block_hash : block_hash option;
    block_number : quantity option;
    transaction_index : quantity option;
    from : address;
    to_ : address option;
    value : quantity;
    gas : quantity;
    max_fee_per_gas : quantity;
    max_priority_fee_per_gas : quantity;
    access_list : access list;
    authorization_list : authorization_item list;
    input : hex;
    v : quantity;
    r : quantity;
    s : quantity;
  }

  let encoding =
    let open Data_encoding in
    conv
      (fun {
             chain_id;
             hash;
             nonce;
             block_hash;
             block_number;
             transaction_index;
             from;
             to_;
             value;
             gas;
             max_fee_per_gas;
             max_priority_fee_per_gas;
             access_list;
             authorization_list;
             input;
             v;
             r;
             s;
           }
         ->
        ( ( chain_id,
            hash,
            nonce,
            block_hash,
            block_number,
            transaction_index,
            from,
            to_,
            value,
            gas ),
          ( max_fee_per_gas,
            max_priority_fee_per_gas,
            Some max_fee_per_gas
            (* Other providers set gasPrice as the effectiveGasPrice, but this
               information is only available in the receipt. *),
            access_list,
            authorization_list,
            input,
            v,
            r,
            s ) ))
      (fun ( ( chain_id,
               hash,
               nonce,
               block_hash,
               block_number,
               transaction_index,
               from,
               to_,
               value,
               gas ),
             ( max_fee_per_gas,
               max_priority_fee_per_gas,
               _gas_price,
               access_list,
               authorization_list,
               input,
               v,
               r,
               s ) )
         ->
        {
          chain_id;
          hash;
          nonce;
          block_hash;
          block_number;
          transaction_index;
          from;
          to_;
          value;
          gas;
          max_fee_per_gas;
          max_priority_fee_per_gas;
          access_list;
          authorization_list;
          input;
          v;
          r;
          s;
        })
      (merge_objs
         (obj10
            (req "chainId" quantity_encoding)
            (req "hash" hash_encoding)
            (req "nonce" quantity_encoding)
            (req "blockHash" (option block_hash_encoding))
            (req "blockNumber" (option quantity_encoding))
            (req "transactionIndex" (option quantity_encoding))
            (req "from" address_encoding)
            (req "to" (option address_encoding))
            (req "value" quantity_encoding)
            (req "gas" quantity_encoding))
         (obj9
            (req "maxFeePerGas" quantity_encoding)
            (req "maxPriorityFeePerGas" quantity_encoding)
            (opt
               "gasPrice"
               quantity_encoding
               ~description:
                 "Identical to maxFeePerGas. Purely for compatibility with \
                  hardhat although the spec specifies this should be null for \
                  EIP 1559.")
            (req "accessList" (list access_encoding))
            (req "authorizationList" (list authorization_item_encoding))
            (req "input" hex_encoding)
            (req "v" quantity_encoding)
            (req "r" quantity_encoding)
            (req "s" quantity_encoding)))
end

type t =
  | Kernel of legacy_transaction_object
      (** Transaction object read from the durable storage. *)
  | Legacy of legacy_transaction_object
  | EIP_2930 of EIP_2930.t
  | EIP_1559 of EIP_1559.t
  | EIP_7702 of EIP_7702.t

let is_eip7702 txn = match txn with EIP_7702 _ -> true | _ -> false

let from_store_transaction_object (obj : legacy_transaction_object) = Kernel obj

let block_from_legacy block =
  {
    block with
    transactions =
      (match block.transactions with
      | TxHash hashes -> TxHash hashes
      | TxFull l -> TxFull (List.map from_store_transaction_object l));
  }

(** [decode_number_be_canonical bytes] decodes [bytes] as a big-endian integer,
    rejecting non-canonical RLP integer encodings, i.e. those carrying a leading
    zero byte.

    The kernel decodes transaction integer fields with the Rust RLP crate's
    [as_val], which enforces this same invariant during blueprint application.
    Accepting a non-canonical integer at admission would therefore let a
    malformed transaction pass the node's public RPC only to abort application
    of the blueprint that embeds it in the kernel (see
    {!Rlp.decode_int}/{!Rlp.decode_z} which enforce the invariant for lengths).
*)
let decode_number_be_canonical bytes =
  let open Result_syntax in
  if Bytes.length bytes > 0 && Bytes.get_uint8 bytes 0 = 0 then
    error_with
      "non-canonical RLP integer encoding in transaction (leading zero byte)"
  else return (decode_number_be bytes)

let decode_number_be_canonical_bounded ~field ~max_bytes bytes =
  if Bytes.length bytes > max_bytes then
    error_with
      "%s is too large for kernel transaction decoding (expected at most %d \
       bytes, got %d)"
      field
      max_bytes
      (Bytes.length bytes)
  else decode_number_be_canonical bytes

let decode_u256_be_canonical ~field bytes =
  decode_number_be_canonical_bounded ~field ~max_bytes:32 bytes

let decode_u64_be_canonical ~field bytes =
  decode_number_be_canonical_bounded ~field ~max_bytes:8 bytes

let decode_u8_be_canonical ~field bytes =
  decode_number_be_canonical_bounded ~field ~max_bytes:1 bytes

let decode_typed_transaction_y_parity bytes =
  let open Result_syntax in
  let* (Qty v as res) = decode_u8_be_canonical ~field:"y_parity" bytes in
  if Compare.Z.(v < Z.of_int 4) then return res
  else error_with "Invalid typed transaction y_parity"

(* The kernel decodes address fields as [H160] (exactly 20 bytes) and storage
   keys as [H256] (exactly 32 bytes). As with {!decode_number_be_canonical},
   accepting a wrong-length value at admission would let a transaction pass the
   node's public RPC only to abort blueprint application in the kernel. *)
let address_length = 20

let storage_key_length = 32

let decode_address_checked bytes =
  let open Result_syntax in
  if Bytes.length bytes = address_length then
    return (Ethereum_types.decode_address bytes)
  else
    error_with
      "invalid address length in transaction (expected %d bytes, got %d)"
      address_length
      (Bytes.length bytes)

let decode_address_checked_or_empty bytes =
  let open Result_syntax in
  if bytes = Bytes.empty then return None
  else
    let+ to_ = decode_address_checked bytes in
    Some to_

let decode_storage_key_checked bytes =
  let open Result_syntax in
  if Bytes.length bytes = storage_key_length then
    return (Ethereum_types.decode_hex bytes)
  else
    error_with
      "invalid storage key length in transaction (expected %d bytes, got %d)"
      storage_key_length
      (Bytes.length bytes)

let decode_access_list =
  let open Result_syntax in
  Rlp.decode_list (function
    | List [Value address; storage_keys] ->
        let* address = decode_address_checked address in
        let* storage_keys =
          Rlp.decode_list
            (function
              | Value x -> decode_storage_key_checked x
              | _ -> error_with "Invalid storage key")
            storage_keys
        in
        Ok {address; storage_keys}
    | _ -> error_with "failed to decode access list from raw transaction")

let decode_authorization_list =
  Rlp.decode_list (function
    | List
        [
          Value chain_id;
          Value address;
          Value nonce;
          Value y_parity;
          Value r;
          Value s;
        ] ->
        let open Result_syntax in
        let* chain_id = decode_u256_be_canonical ~field:"chain_id" chain_id in
        let* address = decode_address_checked address in
        let* nonce = decode_u64_be_canonical ~field:"nonce" nonce in
        let* y_parity = decode_u8_be_canonical ~field:"y_parity" y_parity in
        let* r = decode_u256_be_canonical ~field:"r" r in
        let* s = decode_u256_be_canonical ~field:"s" s in
        return {chain_id; address; nonce; y_parity; r; s}
    | _ -> error_with "Expected list of 6 elements in authorization list")

(** Compute the recovery ID from the signature's [v] value for legacy transactions.
    - If [chain_id] is [None], assumes pre-EIP-155 format ([v] = 27 or 28).
    - If [chain_id] is [Some id], computes the recovery ID using the EIP-155 formula:
      [recovery_id = v - (2 * chain_id + 35)].
*)
let legacy_recovery_id ~chain_id ~v =
  let open Z in
  let recovery_id =
    match chain_id with
    | None -> v - of_int 27
    | Some chain_id -> v - ((chain_id * of_int 2) + of_int 35)
  in
  if recovery_id = zero || recovery_id = one then Ok (to_int recovery_id)
  else Error "Invalid recovery id"

(** Decode the [v] value of a typed transaction as a recovery ID ([0] or [1]).
    Legacy transactions, including the historical [27]/[28] encoding, are
    handled separately by {!legacy_recovery_id}.

    The kernel currently accepts [0..3]; this node-side validation is
    intentionally stricter and keeps the historical node behaviour of rejecting
    [2] and [3]. *)
let typed_recovery_id ~v =
  if Compare.Z.(v = Z.zero || v = Z.one) then Ok (Z.to_int v)
  else Error "Invalid recovery id"

(** Half of the secp256k1 curve order (or subgroup size).
    To avoid two encodings of the same signature, Ethereum enforces a rule: s must be =< to n
    This precomputed value is used to enforce this rule so that signatures are canonical. *)
let secp256k1n_2 =
  Z.of_string
    "0x7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF5D576E7357A4501DDFE92F46681B20A0"

(** Recover the Ethereum address corresponding to a given signature over [hash].
    - [hash] is the 32-byte message hash that was signed.
    - [recovery_id] must be [0] or [1].
    - [r] and [s] are the ECDSA signature components.

    The function:
    - Validates that [s] is in the lower half of the secp256k1 curve order.
    - Pads [r] and [s] to 32 bytes.
    - Calls [Secp256k1.recover] to obtain the public key.
    - Returns the recovered Ethereum address.
*)
let recover_sender ~hash ~recovery_id ~r ~s =
  let open Result_syntax in
  let encode_z_be z =
    let rv = Z.to_bits z in
    Bytes.init (String.length rv) (fun i -> rv.[String.length rv - 1 - i])
  in
  let* recovery_id =
    (* The kernel currently accepts 0, 1, 2, and 3. This keeps the historical
       node behaviour of rejecting 2 and 3. *)
    if recovery_id = 0 || recovery_id = 1 then
      Ok (Bytes.of_string (String.make 1 (Char.chr recovery_id)))
    else Error "Invalid recovery id"
  in
  let* () = if s > secp256k1n_2 then Error "High s value" else Ok () in
  let pad32 z =
    let b = encode_z_be z in
    if Bytes.length b < 32 then
      Bytes.cat (Bytes.make (32 - Bytes.length b) '\000') b
    else b
  in
  let sig_ = Bytes.concat Bytes.empty [pad32 r; pad32 s; recovery_id] in
  let* pub = Tezos_crypto.Signature.Secp256k1.recover sig_ hash in
  Ok (Ethereum_types.Address (Hex (Hex.of_bytes pub |> Hex.show)))

(** Recover the signer of a typed Ethereum transaction.
    - [rlp] is the RLP-encoded transaction.
    - [prefix_char] is the transaction type prefix byte (e.g. [0x02] for EIP-1559).
    - [v], [r], [s] are the signature components.
*)
let transaction_signer ~rlp ~v ~r ~s ~prefix_char =
  let open Result_syntax in
  let module H = Tezos_crypto.Hacl.Hash.Keccak_256 in
  let prefix = Bytes.make 1 (Char.chr prefix_char) in
  let hash = H.digest (Bytes.cat prefix (Rlp.encode rlp)) in
  let* recovery_id = typed_recovery_id ~v in
  recover_sender ~hash ~recovery_id ~r ~s

(** Recover the signer of a legacy (type [0x0]) Ethereum transaction.
    - [rlp] is the RLP-encoded transaction payload.
    - [v], [r], [s] are the signature components.
    - [chain_id] is optional: [None] for pre-EIP-155, [Some id] for EIP-155 signatures.
*)
let legacy_transaction_signer ~rlp ~v ~r ~s ~chain_id =
  let open Result_syntax in
  let module H = Tezos_crypto.Hacl.Hash.Keccak_256 in
  let hash = H.digest @@ Rlp.encode rlp in
  let* recovery_id = legacy_recovery_id ~v ~chain_id in
  recover_sender ~hash ~recovery_id ~r ~s

let reconstruct_eip_2930_from_raw raw_txn hash =
  let open Result_syntax in
  match Rlp.decode (Bytes.unsafe_of_string raw_txn) with
  | Ok
      (List
         [
           Value chain_id;
           Value nonce;
           Value gas_price;
           Value gas_limit;
           Value to_;
           Value value;
           Value input;
           access_list;
           Value v;
           Value r;
           Value s;
         ]) ->
      let recovery_list =
        Rlp.List
          [
            Value chain_id;
            Value nonce;
            Value gas_price;
            Value gas_limit;
            Value to_;
            Value value;
            Value input;
            access_list;
          ]
      in
      let* chain_id = decode_u256_be_canonical ~field:"chain_id" chain_id in
      let* nonce = decode_u64_be_canonical ~field:"nonce" nonce in
      let* gas_price = decode_u256_be_canonical ~field:"gas_price" gas_price in
      let* gas = decode_u64_be_canonical ~field:"gas_limit" gas_limit in
      let* to_ = decode_address_checked_or_empty to_ in
      let* value = decode_u256_be_canonical ~field:"value" value in
      let input = Ethereum_types.decode_hex input in
      let* access_list = decode_access_list access_list in
      let* v = decode_typed_transaction_y_parity v in
      let* r = decode_u256_be_canonical ~field:"r" r in
      let* s = decode_u256_be_canonical ~field:"s" s in
      let (Qty v_z) = v in
      let (Qty r_z) = r in
      let (Qty s_z) = s in
      let* from =
        match
          transaction_signer
            ~rlp:recovery_list
            ~prefix_char:1
            ~v:v_z
            ~r:r_z
            ~s:s_z
        with
        | Ok addr -> Ok addr
        | Error msg -> error_with "%s" msg
      in
      return
        (EIP_2930
           {
             chain_id;
             block_hash = None;
             block_number = None;
             from;
             gas;
             gas_price;
             hash;
             input;
             nonce;
             to_;
             transaction_index = None;
             value;
             access_list;
             v;
             r;
             s;
           })
  | _ -> error_with "failed to decode EIP-2930 transaction"

let reconstruct_eip_1559_from_raw raw_txn hash =
  let open Result_syntax in
  match Rlp.decode (Bytes.unsafe_of_string raw_txn) with
  | Ok
      (List
         [
           Value chain_id;
           Value nonce;
           Value max_priority_fee_per_gas;
           Value max_fee_per_gas;
           Value gas_limit;
           Value to_;
           Value value;
           Value input;
           access_list;
           Value v;
           Value r;
           Value s;
         ]) ->
      let recovery_list =
        Rlp.List
          [
            Value chain_id;
            Value nonce;
            Value max_priority_fee_per_gas;
            Value max_fee_per_gas;
            Value gas_limit;
            Value to_;
            Value value;
            Value input;
            access_list;
          ]
      in
      let* chain_id = decode_u256_be_canonical ~field:"chain_id" chain_id in
      let* nonce = decode_u64_be_canonical ~field:"nonce" nonce in
      let* max_priority_fee_per_gas =
        decode_u256_be_canonical
          ~field:"max_priority_fee_per_gas"
          max_priority_fee_per_gas
      in
      let* max_fee_per_gas =
        decode_u256_be_canonical ~field:"max_fee_per_gas" max_fee_per_gas
      in
      let* gas = decode_u64_be_canonical ~field:"gas_limit" gas_limit in
      let* to_ = decode_address_checked_or_empty to_ in
      let* value = decode_u256_be_canonical ~field:"value" value in
      let input = Ethereum_types.decode_hex input in
      let* access_list = decode_access_list access_list in
      let* v = decode_typed_transaction_y_parity v in
      let* r = decode_u256_be_canonical ~field:"r" r in
      let* s = decode_u256_be_canonical ~field:"s" s in
      let (Qty v_z) = v in
      let (Qty r_z) = r in
      let (Qty s_z) = s in
      let* from =
        match
          transaction_signer
            ~rlp:recovery_list
            ~prefix_char:2
            ~v:v_z
            ~r:r_z
            ~s:s_z
        with
        | Ok addr -> Ok addr
        | Error msg -> error_with "%s" msg
      in
      return
        (EIP_1559
           {
             chain_id;
             hash;
             nonce;
             block_hash = None;
             block_number = None;
             transaction_index = None;
             from;
             to_;
             value;
             gas;
             max_fee_per_gas;
             max_priority_fee_per_gas;
             access_list;
             input;
             v;
             r;
             s;
           })
  | _ -> error_with "Invalid EIP-1559 transaction"

let reconstruct_eip_7702_from_raw raw_txn hash =
  let open Result_syntax in
  match Rlp.decode (Bytes.unsafe_of_string raw_txn) with
  | Ok
      (List
         [
           Value chain_id;
           Value nonce;
           Value max_priority_fee_per_gas;
           Value max_fee_per_gas;
           Value gas_limit;
           Value to_;
           Value value;
           Value input;
           access_list;
           authorization_list;
           Value v;
           Value r;
           Value s;
         ]) ->
      let recovery_list =
        Rlp.List
          [
            Value chain_id;
            Value nonce;
            Value max_priority_fee_per_gas;
            Value max_fee_per_gas;
            Value gas_limit;
            Value to_;
            Value value;
            Value input;
            access_list;
            authorization_list;
          ]
      in
      let* chain_id = decode_u256_be_canonical ~field:"chain_id" chain_id in
      let* nonce = decode_u64_be_canonical ~field:"nonce" nonce in
      let* max_priority_fee_per_gas =
        decode_u256_be_canonical
          ~field:"max_priority_fee_per_gas"
          max_priority_fee_per_gas
      in
      let* max_fee_per_gas =
        decode_u256_be_canonical ~field:"max_fee_per_gas" max_fee_per_gas
      in
      let* gas = decode_u64_be_canonical ~field:"gas_limit" gas_limit in
      let* to_ = decode_address_checked_or_empty to_ in
      let* value = decode_u256_be_canonical ~field:"value" value in
      let input = decode_hex input in
      let* access_list = decode_access_list access_list in
      let* authorization_list = decode_authorization_list authorization_list in
      let* v = decode_typed_transaction_y_parity v in
      let* r = decode_u256_be_canonical ~field:"r" r in
      let* s = decode_u256_be_canonical ~field:"s" s in
      let (Qty v_z) = v in
      let (Qty r_z) = r in
      let (Qty s_z) = s in
      let* from =
        match
          transaction_signer
            ~rlp:recovery_list
            ~prefix_char:4
            ~v:v_z
            ~r:r_z
            ~s:s_z
        with
        | Ok addr -> Ok addr
        | Error msg -> error_with "%s" msg
      in
      return
        (EIP_7702
           {
             chain_id;
             hash;
             nonce;
             block_hash = None;
             block_number = None;
             transaction_index = None;
             from;
             to_;
             value;
             gas;
             max_fee_per_gas;
             max_priority_fee_per_gas;
             access_list;
             authorization_list;
             input;
             v;
             r;
             s;
           })
  | _ -> error_with "failed to decode EIP-7702 transaction"

let decode_legacy_chain_id ~v =
  let open Z in
  let open Result_syntax in
  if v > of_int 36 then return (Some (div (v - of_int 35) (of_int 2)))
  else if v = of_int 27 || v = of_int 28 then return None
  else error_with "Legacy transaction chain ID cannot be decoded"

let reconstruct_legacy_from_raw raw_txn hash =
  let open Result_syntax in
  match Rlp.decode (Bytes.unsafe_of_string raw_txn) with
  | Ok
      (List
         [
           Value nonce;
           Value gas_price;
           Value gas_limit;
           Value to_;
           Value value;
           Value input;
           Value v;
           Value r;
           Value s;
         ]) ->
      let recovery_list =
        Rlp.
          [
            Value nonce;
            Value gas_price;
            Value gas_limit;
            Value to_;
            Value value;
            Value input;
          ]
      in
      let* nonce = decode_u64_be_canonical ~field:"nonce" nonce in
      let* gasPrice = decode_u256_be_canonical ~field:"gas_price" gas_price in
      let* gas = decode_u64_be_canonical ~field:"gas_limit" gas_limit in
      let* to_ = decode_address_checked_or_empty to_ in
      let* value = decode_u256_be_canonical ~field:"value" value in
      let input = Ethereum_types.decode_hex input in
      let* v = decode_u256_be_canonical ~field:"v" v in
      let* r = decode_u256_be_canonical ~field:"r" r in
      let* s = decode_u256_be_canonical ~field:"s" s in
      let (Qty v_z) = v in
      let (Qty r_z) = r in
      let (Qty s_z) = s in

      let* chain_id = decode_legacy_chain_id ~v:v_z in
      let signature =
        match chain_id with
        | Some id ->
            (* If the chain id is present we are post EIP155, the chain id
                 is the `v` field of the `v * r * s` where `r` and `s` are
                 empty bytes. *)
            Rlp.[Value (encode_z id); Value Bytes.empty; Value Bytes.empty]
        | None ->
            (* If we are pre EIP155, there's simply no signature. *)
            []
      in
      let recovery_list = Rlp.List (recovery_list @ signature) in

      let* from =
        match
          legacy_transaction_signer
            ~rlp:recovery_list
            ~chain_id
            ~v:v_z
            ~r:r_z
            ~s:s_z
        with
        | Ok addr -> Ok addr
        | Error msg -> error_with "%s" msg
      in
      return
        (Legacy
           {
             hash;
             nonce;
             blockHash = None;
             blockNumber = None;
             transactionIndex = None;
             from;
             to_;
             value;
             gas;
             gasPrice;
             input;
             v;
             r;
             s;
           })
  | _ -> error_with "failed to decode legacy transaction"

let reconstruct_from_eip_2930_transaction (obj : legacy_transaction_object)
    raw_txn =
  let open Result_syntax in
  let* tx = reconstruct_eip_2930_from_raw raw_txn obj.hash in
  match tx with
  | EIP_2930 eip ->
      return
        (EIP_2930
           {
             eip with
             block_hash = obj.blockHash;
             block_number = obj.blockNumber;
             transaction_index = obj.transactionIndex;
             from = obj.from;
           })
  | _ ->
      error_with
        "Unexpected transaction type in reconstruct_from_eip_2930_transaction"

let reconstruct_from_eip_1559_transaction (obj : legacy_transaction_object)
    raw_txn =
  let open Result_syntax in
  let* tx = reconstruct_eip_1559_from_raw raw_txn obj.hash in
  match tx with
  | EIP_1559 eip ->
      return
        (EIP_1559
           {
             eip with
             block_hash = obj.blockHash;
             block_number = obj.blockNumber;
             transaction_index = obj.transactionIndex;
             from = obj.from;
           })
  | _ ->
      error_with
        "Unexpected transaction type in reconstruct_from_eip_1559_transaction"

let reconstruct_from_eip_7702_transaction (obj : legacy_transaction_object)
    raw_txn =
  let open Result_syntax in
  let* tx = reconstruct_eip_7702_from_raw raw_txn obj.hash in
  match tx with
  | EIP_7702 eip ->
      return
        (EIP_7702
           {
             eip with
             block_hash = obj.blockHash;
             block_number = obj.blockNumber;
             transaction_index = obj.transactionIndex;
             from = obj.from;
           })
  | _ ->
      error_with
        "Unexpected transaction type in reconstruct_from_eip_7702_transaction"

let reconstruct_from_raw_transaction (obj : legacy_transaction_object) raw_txn =
  if String.length raw_txn = 0 then error_with "empty raw transaction"
  else
    match String.get raw_txn 0 with
    | '\x04' ->
        (* EIP 7702 *)
        reconstruct_from_eip_7702_transaction
          obj
          (String.sub raw_txn 1 (String.length raw_txn - 1))
    | '\x02' ->
        (* EIP 1559 *)
        reconstruct_from_eip_1559_transaction
          obj
          (String.sub raw_txn 1 (String.length raw_txn - 1))
    | '\x01' ->
        (* EIP 2930 *)
        reconstruct_from_eip_2930_transaction
          obj
          (String.sub raw_txn 1 (String.length raw_txn - 1))
    | _ -> Ok (Legacy obj)

let decode raw_txn =
  if String.length raw_txn = 0 then error_with "empty raw transaction"
  else
    let hash = Ethereum_types.hash_raw_tx raw_txn in
    match String.get raw_txn 0 with
    | '\x04' ->
        reconstruct_eip_7702_from_raw
          (String.sub raw_txn 1 (String.length raw_txn - 1))
          hash
    | '\x02' ->
        reconstruct_eip_1559_from_raw
          (String.sub raw_txn 1 (String.length raw_txn - 1))
          hash
    | '\x01' ->
        reconstruct_eip_2930_from_raw
          (String.sub raw_txn 1 (String.length raw_txn - 1))
          hash
    | _ -> reconstruct_legacy_from_raw raw_txn hash

let reconstruct_from_transactions_list transactions
    (obj : legacy_transaction_object) =
  let open Result_syntax in
  match List.assoc ~equal:( = ) obj.hash transactions with
  | Some None | None ->
      (* The raw transaction is unavailable: either it is a non-EVM delayed
         transaction (deposit, FA deposit, Tezos operation), which cannot be
         reconstructed, or its payload is missing. We return the potentially
         incorrect transaction object as stored. *)
      return (from_store_transaction_object obj)
  | Some (Some (Broadcast.Evm raw_txn)) ->
      (* We have the original transaction, let's try to reconstruct the
         transaction object *)
      let* res = reconstruct_from_raw_transaction obj raw_txn in
      return res
  | Some (Some (Broadcast.Michelson _)) ->
      (* Reconstructing a Tezos operation does not make sense. *)
      error_with "Tezos operations cannot be reconstructed"

(** [transactions_of_blueprint bwe] returns the association list mapping every
    transaction hash of the blueprint to its raw transaction (when available).

    The blueprint payload only contains the raw transactions injected by the
    sequencer; for delayed transactions it merely references their hash. We
    therefore complete the list with the raw transactions of the delayed EVM
    transactions, which are carried by [bwe.delayed_transactions]. These entries
    are placed first so that they shadow the [(hash, None)] entries the payload
    holds for the same delayed transactions. Non-EVM delayed transactions
    (deposits, FA deposits, Tezos operations) have no raw EVM transaction to
    reconstruct and are left out. *)
let transactions_of_blueprint (bwe : Blueprint_types.with_events) =
  let open Result_syntax in
  let Blueprint_types.{blueprint; delayed_transactions; _} = bwe in
  let* payload_transactions =
    Blueprint_decoder.transactions blueprint.payload
  in
  let delayed_transactions =
    List.filter_map
      (fun (delayed : Evm_events.Delayed_transaction.t) ->
        match delayed.kind with
        | Evm_events.Delayed_transaction.EthereumTransaction ->
            Some
              ( Ethereum_types.hash_raw_tx delayed.raw,
                Some (Broadcast.Evm delayed.raw) )
        | Deposit | Fa_deposit | TezosOperation -> None)
      delayed_transactions
  in
  return (delayed_transactions @ payload_transactions)

let reconstruct bwe (obj : legacy_transaction_object) =
  let open Result_syntax in
  let* transactions = transactions_of_blueprint bwe in
  reconstruct_from_transactions_list transactions obj

let reconstruct_block bwe block =
  let open Result_syntax in
  let* transactions = transactions_of_blueprint bwe in
  match block.transactions with
  | TxHash h -> return {block with transactions = TxHash h}
  | TxFull l ->
      let* l = List.map_e (reconstruct_from_transactions_list transactions) l in
      return {block with transactions = TxFull l}

let rereconstruct bwe = function
  | Kernel obj -> reconstruct bwe obj
  | other -> Ok other

let rereconstruct_block bwe block =
  let open Result_syntax in
  let* transactions = transactions_of_blueprint bwe in
  match block.transactions with
  | TxHash h -> return {block with transactions = TxHash h}
  | TxFull l ->
      let* l =
        List.map_e
          (function
            | Kernel obj -> reconstruct_from_transactions_list transactions obj
            | other -> Ok other)
          l
      in
      return {block with transactions = TxFull l}

let hash = function
  | Kernel obj -> obj.hash
  | Legacy obj -> obj.hash
  | EIP_2930 obj -> obj.hash
  | EIP_1559 obj -> obj.hash
  | EIP_7702 obj -> obj.hash

let block_number = function
  | Kernel obj -> obj.blockNumber
  | Legacy obj -> obj.blockNumber
  | EIP_2930 obj -> obj.block_number
  | EIP_1559 obj -> obj.block_number
  | EIP_7702 obj -> obj.block_number

let input = function
  | Kernel obj -> obj.input
  | Legacy obj -> obj.input
  | EIP_2930 obj -> obj.input
  | EIP_1559 obj -> obj.input
  | EIP_7702 obj -> obj.input

let to_ = function
  | Kernel obj -> obj.to_
  | Legacy obj -> obj.to_
  | EIP_2930 obj -> obj.to_
  | EIP_1559 obj -> obj.to_
  | EIP_7702 obj -> obj.to_

let sender = function
  | Kernel {from; _}
  | Legacy {from; _}
  | EIP_2930 {from; _}
  | EIP_1559 {from; _}
  | EIP_7702 {from; _} ->
      from

let nonce = function
  | Kernel {nonce; _}
  | Legacy {nonce; _}
  | EIP_2930 {nonce; _}
  | EIP_1559 {nonce; _}
  | EIP_7702 {nonce; _} ->
      nonce

let block_hash = function
  | Kernel obj -> obj.blockHash
  | Legacy obj -> obj.blockHash
  | EIP_2930 obj -> obj.block_hash
  | EIP_1559 obj -> obj.block_hash
  | EIP_7702 obj -> obj.block_hash

let transaction_index = function
  | Kernel obj -> obj.transactionIndex
  | Legacy obj -> obj.transactionIndex
  | EIP_2930 obj -> obj.transaction_index
  | EIP_1559 obj -> obj.transaction_index
  | EIP_7702 obj -> obj.transaction_index

let value = function
  | Kernel {value; _}
  | Legacy {value; _}
  | EIP_2930 {value; _}
  | EIP_1559 {value; _}
  | EIP_7702 {value; _} ->
      value

let gas = function
  | Kernel {gas; _}
  | Legacy {gas; _}
  | EIP_2930 {gas; _}
  | EIP_1559 {gas; _}
  | EIP_7702 {gas; _} ->
      gas

let access_list = function
  | Kernel _ | Legacy _ -> []
  | EIP_2930 {access_list; _}
  | EIP_1559 {access_list; _}
  | EIP_7702 {access_list; _} ->
      access_list

let authorization_list = function
  | Kernel _ | Legacy _ | EIP_2930 _ | EIP_1559 _ -> []
  | EIP_7702 {authorization_list; _} -> authorization_list

let chain_id =
  let open Result_syntax in
  function
  | Kernel {v = Qty v; _} | Legacy {v = Qty v; _} ->
      let* chain_id = decode_legacy_chain_id ~v in
      Ok (Option.map (fun z -> Qty z) chain_id)
  | EIP_2930 {chain_id; _} | EIP_1559 {chain_id; _} | EIP_7702 {chain_id; _} ->
      Ok (Some chain_id)

let max_fee_per_gas = function
  | Kernel obj -> obj.gasPrice
  | Legacy obj -> obj.gasPrice
  | EIP_2930 obj -> obj.gas_price
  | EIP_1559 obj -> obj.max_fee_per_gas
  | EIP_7702 obj -> obj.max_fee_per_gas

let encoding =
  let open Data_encoding in
  union
    [
      case
        ~title:"legacy"
        (Tag 0)
        Ethereum_types.legacy_transaction_object_encoding
        (function Legacy o -> Some o | _ -> None)
        (fun o -> Legacy o);
      case
        ~title:"eip-2930"
        (Tag 3)
        (merge_objs (obj1 (req "type" (constant "0x1"))) EIP_2930.encoding)
        (function EIP_2930 o -> Some ((), o) | _ -> None)
        (fun ((), o) -> EIP_2930 o);
      case
        ~title:"eip-1559"
        (Tag 4)
        (merge_objs (obj1 (req "type" (constant "0x2"))) EIP_1559.encoding)
        (function EIP_1559 o -> Some ((), o) | _ -> None)
        (fun ((), o) -> EIP_1559 o);
      case
        ~title:"eip-7702"
        (Tag 5)
        (merge_objs (obj1 (req "type" (constant "0x4"))) EIP_7702.encoding)
        (function EIP_7702 o -> Some ((), o) | _ -> None)
        (fun ((), o) -> EIP_7702 o);
      case
        ~title:"kernel"
        (Tag 255)
        Ethereum_types.legacy_transaction_object_encoding
        (function Kernel o -> Some o | _ -> None)
        (fun o -> Kernel o);
      (* The following are incorrect but kept for backward compatibility. See
         https://ethereum.org/en/developers/docs/apis/json-rpc/#quantities-encoding
         and
         https://ethereum.org/en/developers/docs/transactions/#typed-transaction-envelope. *)
      case
        ~title:"eip-2930-backward-compat"
        (Tag 1)
        (merge_objs (obj1 (req "type" (constant "0x01"))) EIP_2930.encoding)
        (fun _ -> None)
        (fun ((), o) -> EIP_2930 o);
      case
        ~title:"eip-1559-backward-compat"
        (Tag 2)
        (merge_objs (obj1 (req "type" (constant "0x02"))) EIP_1559.encoding)
        (fun _ -> None)
        (fun ((), o) -> EIP_1559 o);
    ]

type txqueue_content = {
  pending : t NonceMap.t AddressMap.t;
  queued : t NonceMap.t AddressMap.t;
}

let txqueue_content_encoding =
  let open Data_encoding in
  let field_encoding =
    AddressMap.associative_array_encoding
      (NonceMap.associative_array_encoding encoding)
  in
  conv
    (fun {pending; queued} -> (pending, queued))
    (fun (pending, queued) -> {pending; queued})
    (obj2 (req "pending" field_encoding) (req "queued" field_encoding))
