(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Protocol
open Environment
open Error_monad
open Alpha_context

val errors :
  'a #RPC_context.simple -> 'a -> Data_encoding.json_schema shell_tzresult Lwt.t

(** Returns all the constants of the protocol *)
val all : 'a #RPC_context.simple -> 'a -> Constants.t shell_tzresult Lwt.t

(** Returns the parametric constants of the protocol *)
val parametric :
  'a #RPC_context.simple -> 'a -> Constants.Parametric.t shell_tzresult Lwt.t

val register : unit -> unit

module S : sig
  val errors :
    ( [`GET],
      Updater.rpc_context,
      Updater.rpc_context,
      unit,
      unit,
      Data_encoding.json_schema )
    RPC_service.t

  val all :
    ( [`GET],
      Updater.rpc_context,
      Updater.rpc_context,
      unit,
      unit,
      Constants.t )
    RPC_service.t

  val parametric :
    ( [`GET],
      Updater.rpc_context,
      Updater.rpc_context,
      unit,
      unit,
      Constants.Parametric.t )
    RPC_service.t
end
