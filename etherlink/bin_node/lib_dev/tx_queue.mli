(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** The Tx_queue is a worker allowing to batch raw transactions in a single
    [eth_sendRawTransaction] at a regular interval. It provides a non-blocking
    interface based on the use of callbacks. *)

(** A [callback] is called by the [Tx_queue] at various stages of a
    submitted transaction's life.

    The next tick after its insertion in the queue, a transaction is submitted
    to the relay node within a batch of [eth_sendRawTransaction] requests.

    {ul
      {li Depending on the result of the RPC, its [callback] is called with
          either [`Accepted hash] (where [hash] is the hash of the raw
          transaction) or [`Refused]).}
      {li As soon as the transaction appears in a blueprint, its callback is
          called with [`Confirmed]. If this does not happen before 2s, the
          [callback] is called with [`Dropped].}} *)
type callback =
  [`Accepted of Ethereum_types.hash | `Confirmed | `Dropped | `Refused] ->
  unit Lwt.t

(** A [request] submitted to the [Tx_queue] consists in a payload
    (that is, a raw transaction) and a {!type:callback} that will be
    used to advertise the transaction's life cycle. *)

type request = {payload : Ethereum_types.hex; callback : callback}

(** [start ~evm_node_endpoint ~max_transaction_batch_length ()] starts
    the worker, meaning it is possible to call {!inject}, {!confirm}
    and {!beacon}. *)
val start :
  config:Configuration.tx_queue ->
  evm_node_endpoint:Uri.t ->
  unit ->
  unit tzresult Lwt.t

(** [shutdown ()] stops the tx queue, waiting for the ongoing request
    to be processed. *)
val shutdown : unit -> unit tzresult Lwt.t

(** [inject ?callback tx_object raw_txn] pushes the raw transaction
    [raw_txn] to the worker queue.

    {b Note:} The promise will be sleeping until at least {!start} is called. *)
val inject :
  ?callback:callback ->
  Ethereum_types.legacy_transaction_object ->
  Ethereum_types.hex ->
  unit tzresult Lwt.t

(** [confirm hash] is to be called by an external component to advertise a
    transaction has been included in a blueprint. *)
val confirm : Ethereum_types.hash -> unit tzresult Lwt.t

(** [beacon ~tick_interval] is a never fulfilled promise which triggers a tick
    in the [Tx_queue] every [tick_interval] seconds. *)
val beacon : tick_interval:float -> unit tzresult Lwt.t
