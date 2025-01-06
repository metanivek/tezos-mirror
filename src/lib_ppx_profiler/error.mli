(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type error =
  | Invalid_action of string
  | Invalid_payload
  | Invalid_aggregate of Key.t
  | Invalid_custom of Key.t
  | Invalid_mark of Key.t
  | Invalid_record of Key.t
  | Invalid_span of Key.t
  | Invalid_stop of Key.t
  | Invalid_list_of_driver_ids of Ppxlib.expression list
  | Improper_field of (Longident.t Location.loc * Ppxlib.expression)
  | Improper_list_field of (Longident.t Location.loc * Ppxlib.expression)
  | Improper_let_binding of Ppxlib.expression
  | Improper_record of (Ppxlib.Ast.longident_loc * Ppxlib.expression) list
  | Malformed_attribute of Ppxlib.expression
  | No_verbosity of Key.t

(** Raise a located error *)
val error : Location.t -> error -> 'a
