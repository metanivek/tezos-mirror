(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** The [Event_loop] module provides an abstraction layer on top of promises
    for use in the Octez codebase.

    Some components will use [eio*] libs, and if your library or binary uses
    these components, you *MUST* use this module instead of the traditional
    [Lwt_main.run].
*)

exception Not_initialized

(** Retrieve the Eio environment for the current [main_run] being executed.
    The returned environment must not escape the scope of this [main_run]
    execution. *)
val env : unit -> Eio_unix.Stdenv.base option

(** Same as [env], but raises [Not_initialized] if called outside of
    [main_run]. *)
val env_exn : unit -> Eio_unix.Stdenv.base

(** Retrieve the main switch for the current [main_run] being executed.
    The returned switch must not escape the scope of this [main_run]
    execution. *)
val main_switch : unit -> Eio.Switch.t option

(** Same as [main_switch] but raises [Not_initialized] if called outside
    of [main_run] execution. *)
val main_switch_exn : unit -> Eio.Switch.t

(** [main_run] should be used as a replacement for [Lwt_main.run], as it also
    handles `Eio` env initialization internal calls to the `Eio` event loop
    if [~eio] is set to [true] ([false] by default).

    You can't nest [main_run] calls. *)
val main_run : ?eio:bool -> (unit -> 'a Lwt.t) -> 'a
