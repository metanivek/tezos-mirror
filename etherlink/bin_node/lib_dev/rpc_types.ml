(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(* The set of supported RPCs depends on the mode.
   For the sequencer, only very few JSON RPCs need to be supported.
   For the observer, proxy, and RPC modes, we assume that a single
   chain is followed even when the multichain feature is activated
   and the set of supported RPCs depends on the chain family. *)

type rpc_server_family =
  | Multichain_sequencer_rpc_server
  | Single_chain_node_rpc_server of Ethereum_types.chain_family
