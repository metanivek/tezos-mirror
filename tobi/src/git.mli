(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Ask Git to do stuff. *)

open Misc

(** Result of checkout out a repository partially.

    [tmp_worktree] is the place where the root of the Git worktree was checked out.

    [tmp_path] is the path where the [path] argument of the checkout function
    was checked out.

    [cleanup] must eventually be called to remove files that were checked out. *)
type checkout_result = {
  tmp_worktree : string;
  tmp_path : string;
  cleanup : unit -> unit;
}

(** Checkout a repository partially, do something, and cleanup.

    [git_reference] is the reference of the commit to checkout.
    For instance it can be a commit hash, a branch name, a tag, or something like [HEAD^^].

    [path] is the main path to checkout.
    The place where it is checked out is returned in [tmp_path].

    [other_paths] are other paths to checkout.
    They are available, like [path], in [tmp_worktree].

    The last argument is a function to call once the files have been checked out.
    After it returns, the temporary worktree is recursively deleted. *)
val with_checkout_into_tmp :
  git_reference:string ->
  path:string ->
  ?other_paths:string list ->
  (checkout_result -> ('a, ([> `failed] as 'b) error) result) ->
  ('a, 'b error) result
