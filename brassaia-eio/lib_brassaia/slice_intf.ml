(*
 * Copyright (c) 2013-2022 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

module type S = sig
  (** {1 Slices} *)

  (** The type for slices. *)
  type t [@@deriving brassaia]

  (** The type for exported contents. *)
  type contents [@@deriving brassaia]

  (** The type for exported nodes. *)
  type node [@@deriving brassaia]

  (** The type for exported commits. *)
  type commit [@@deriving brassaia]

  (** The type for exported values. *)
  type value = [`Contents of contents | `Node of node | `Commit of commit]
  [@@deriving brassaia]

  (** Create a new empty slice. *)
  val empty : unit -> t

  (** [add t v] adds [v] to [t]. *)
  val add : t -> value -> unit

  (** [iter t f] calls [f] on all values of [t]. *)
  val iter : t -> (value -> unit) -> unit
end

module type Sigs = sig
  (** The signature for slices. *)
  module type S = S

  (** Build simple slices. *)
  module Make (C : Contents.Store) (N : Node.Store) (H : Commit.Store) :
    S
      with type contents = C.hash * C.value
       and type node = N.hash * N.value
       and type commit = H.hash * H.value
end
