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

(** In-memory store.

    A simple in-memory store, using hash tables. Once one of the functors below
    is instantiated to a module [M], it has a unique shared hash-table: multiple
    invocation of [M.create] will see and manipulate the same contents. *)

module Brassaia = Brassaia_eio.Brassaia

module Conf : sig
  val spec : Brassaia.Backend.Conf.Spec.t
end

(** Configuration values. *)
val config : unit -> Brassaia.config

(** An in-memory store for append-only values. *)
module Append_only : Brassaia.Append_only.Maker

(** An in-memory store for content-addressable values. *)
module Content_addressable : Brassaia.Content_addressable.Maker

(** An in-memory store with atomic-write guarantees. *)
module Atomic_write : Brassaia.Atomic_write.Maker

(** Constructor for in-memory KV stores. *)
module KV :
  Brassaia.KV_maker
    with type endpoint = unit
     and type info = Brassaia.Info.default

(** Constructor for in-memory Brassaia store. *)
include Brassaia.Maker with type endpoint = unit
