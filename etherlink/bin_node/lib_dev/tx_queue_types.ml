(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module type L2_transaction = sig
  type t

  type legacy

  type address

  type nonce

  val address_encoding : address Data_encoding.t

  val hash_of_tx_object : legacy -> Ethereum_types.hash

  val address_to_string : address -> string

  val from_address_of_tx_object : legacy -> address

  val bitset_add_nonce : Nonce_bitset.t -> nonce -> Nonce_bitset.t tzresult

  val bitset_remove_nonce : Nonce_bitset.t -> nonce -> Nonce_bitset.t tzresult

  val next_nonce : nonce -> Z.t

  val nonce_to_z_opt : nonce -> Z.t option

  val nonce_of_tx_object : legacy -> nonce

  val transaction_object_from_legacy : legacy -> t

  module AddressMap : Map.S with type key = address

  val make_txpool :
    pending:legacy Ethereum_types.NonceMap.t AddressMap.t ->
    queued:legacy Ethereum_types.NonceMap.t AddressMap.t ->
    Ethereum_types.txpool
end

module Eth_transaction_object :
  L2_transaction
    with type t = Transaction_object.t
     and type legacy = Ethereum_types.legacy_transaction_object
     and type address = Ethereum_types.address
     and module AddressMap = Ethereum_types.AddressMap
     and type nonce = Ethereum_types.quantity = struct
  open Ethereum_types

  type t = Transaction_object.t

  type legacy = legacy_transaction_object

  type nonrec address = address

  type nonce = quantity

  let address_encoding = address_encoding

  let hash_of_tx_object (tx_object : legacy_transaction_object) = tx_object.hash

  let address_to_string (Address (Hex s)) = s

  let from_address_of_tx_object (tx_object : legacy_transaction_object) =
    tx_object.from

  let bitset_add_nonce bitset (Qty nonce) = Nonce_bitset.add bitset ~nonce

  let bitset_remove_nonce bitset (Qty nonce) = Nonce_bitset.remove bitset ~nonce

  let next_nonce (Qty nonce) = Z.succ nonce

  let nonce_to_z_opt (Qty nonce) = Some nonce

  let nonce_of_tx_object (tx_object : legacy_transaction_object) =
    tx_object.nonce

  let transaction_object_from_legacy =
    Transaction_object.from_store_transaction_object

  module AddressMap = AddressMap

  let make_txpool ~pending ~queued = {pending; queued}
end

type tezlink_batch_nonces = {first : Z.t; length : int}

module Tezlink_operation :
  L2_transaction
    with type t = Tezos_types.Operation.t
     and type legacy = Tezos_types.Operation.t
     and type nonce = tezlink_batch_nonces = struct
  open Ethereum_types

  type t = Tezos_types.Operation.t

  type legacy = Tezos_types.Operation.t

  type address = Signature.public_key_hash

  type nonce = tezlink_batch_nonces

  let address_encoding = Signature.Public_key_hash.encoding

  let hash_of_tx_object = Tezos_types.Operation.hash_operation

  let address_to_string = Signature.Public_key_hash.to_string

  let from_address_of_tx_object (op : Tezos_types.Operation.t) = op.source

  let bitset_add_nonce bitset {first; length} =
    Nonce_bitset.add_many bitset ~nonce:first ~length

  let bitset_remove_nonce bitset {first; length} =
    Nonce_bitset.remove_many bitset ~nonce:first ~length

  let next_nonce {first; length} = Z.(add first (of_int length))

  (* This function is only called in the handler of the [Content]
     request which is never called in the case of Tezlink. *)
  let nonce_to_z_opt _nonce = None

  let nonce_of_tx_object (op : Tezos_types.Operation.t) =
    {first = op.first_counter; length = op.length}

  let transaction_object_from_legacy op = op

  module AddressMap = Map.Make (Signature.Public_key_hash)

  let make_txpool ~pending:_ ~queued:_ =
    {
      pending = Ethereum_types.AddressMap.empty;
      queued = Ethereum_types.AddressMap.empty;
    }
end
