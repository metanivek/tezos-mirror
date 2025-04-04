(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** [request_uri ~node_addr ~uri] is a raw call that will return the
    Cohttp response of an RPC call, given a [~uri], against the
    [~node_addr]. *)
val request_uri :
  node_addr:string ->
  uri:string ->
  (Cohttp_lwt_unix.Response.t * Cohttp_lwt.Body.t) tzresult Lwt.t

(** [get_level ~node_addr] returns the level of the block. *)
val get_level : node_addr:string -> (int, error trace) result Lwt.t

(** [get_level ~node_addr] returns the hash of the block. *)
val get_block_hash :
  node_addr:string -> (Block_hash.t, error trace) result Lwt.t

(** [get_next_protocol_hash ~node_addr] returns the protocol hash
    contained in the [next_protocol] field of the metadata of a
    block. *)
val get_next_protocol_hash : node_addr:string -> Protocol_hash.t tzresult Lwt.t

(** [get_current_period ~node_addr] returns the current voting
    period in addition to the number of remaining blocks until the end
    of the period. *)
val get_current_period : node_addr:string -> (string * int) tzresult Lwt.t
