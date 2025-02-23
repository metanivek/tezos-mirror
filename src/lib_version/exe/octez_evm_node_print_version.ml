(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(* This is a script run at build time to print out the current version of Etherlink. *)

open Current_git_info

let () = Print_version.print_version octez_evm_node_version
