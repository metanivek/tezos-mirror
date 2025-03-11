(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(* Review note: might have to rename this, I was not very inspired kekw *)
type validation_mode =
  | Stateless
      (** Minimal validation, includes chain id, nonce, and sender checks *)
  | With_state  (** State related validation, includes fees and gas checks *)
  | Full  (** Combination of both *)

(** [is_tx_valid backend_rpc tx_raw] validates the transaction
    [tx_raw] and returns the next allowed nonce for the sender of the
    transaction alongside the transaction object. *)
val is_tx_valid :
  (module Services_backend_sig.S) ->
  mode:validation_mode ->
  string ->
  ( Ethereum_types.quantity * Ethereum_types.legacy_transaction_object,
    string )
  result
  tzresult
  Lwt.t
