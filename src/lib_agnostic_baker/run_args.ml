(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(* Arguments for which we do not need to run a baker process. *)

let help_arg = "--help"

let version_arg = "--version"

let man_arg = "man"

let is_help_cmd = List.mem ~equal:String.equal help_arg

let is_version_cmd = List.mem ~equal:String.equal version_arg

let is_man_cmd = List.mem ~equal:String.equal man_arg

(* Arguments needed for the start and monitoring of the agnostic baker process. *)

let endpoint_arg = "--endpoint"

let endpoint_short_arg = "-E"

let base_dir_arg = "--base-dir"

let base_dir_short_arg = "-d"

(** Retrieves the value for a given argument key. It checks both the long and short forms. *)
let get_arg_value ~arg ?(short_arg = "") =
  let rec loop = function
    | [] -> None
    | x :: y :: _ when x = arg || x = short_arg -> Some y
    | _ :: l -> loop l
  in
  loop

let get_endpoint args =
  Option.value ~default:Parameters.default_node_endpoint
  @@ get_arg_value ~arg:endpoint_arg ~short_arg:endpoint_short_arg args

let get_base_dir = get_arg_value ~arg:base_dir_arg ~short_arg:base_dir_short_arg
