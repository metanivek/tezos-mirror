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

open! Import

module type S = sig
  (** {1 Signature for store contents} *)

  (** The type for user-defined contents. *)
  type t [@@deriving brassaia]

  (** Merge function. Evaluates to [`Conflict msg] if the values cannot be
      merged properly. The arguments of the merge function can take [None] to
      mean that the key does not exists for either the least-common ancestor or
      one of the two merging points. The merge function returns [None] when the
      key's value should be deleted. *)
  val merge : t option Merge.t

  (** [encoding] is the data_encoding value for {!type-t}. *)
  val encoding : t Data_encoding.t
end

module type Store = sig
  include Indexable.S

  (** [merge t] lifts the merge functions defined on contents values to contents
      key. The merge function will: {e (i)} read the values associated with the
      given keys, {e (ii)} use the merge function defined on values and
      {e (iii)} write the resulting values into the store to get the resulting
      key. See {!module-type-S.val-merge}.

      If any of these operations fail, return [`Conflict]. *)
  val merge : [> read_write] t -> key option Merge.t

  module Val : S with type t = value

  module Hash : Hash.Typed with type t = hash and type value = value
end

module type Sigs = sig
  module type S = S

  (** Contents of type [string], with the {{!Brassaia.Merge.default} default} 3-way
      merge strategy: assume that update operations are idempotent and conflict
      iff values are modified concurrently. *)
  module String : S with type t = string

  (** Similar to [String] above, but the hash computation is compatible with
      versions older than brassaia.3.0 *)
  module String_v2 : S with type t = string

  type json =
    [ `Null
    | `Bool of bool
    | `String of string
    | `Float of float
    | `O of (string * json) list
    | `A of json list ]

  (** [Json] contents are associations from strings to [json] values stored as
      JSON encoded strings. If the same JSON key has been modified concurrently
      with different values then the [merge] function conflicts. *)
  module Json : S with type t = (string * json) list

  (** [Json_value] allows any kind of json value to be stored, not only objects. *)
  module Json_value : S with type t = json

  module V1 : sig
    (** Same as {!String} but use v1 serialisation format. *)
    module String : S with type t = string
  end

  (** Contents store. *)
  module type Store = Store

  (** [Store] creates a contents store. *)
  module Store
      (S : Content_addressable.S)
      (H : Hash.S with type t = S.key)
      (C : S with type t = S.value) :
    Store
      with type 'a t = 'a S.t
       and type key = H.t
       and type hash = H.t
       and type value = C.t

  (** [Store_indexable] is like {!module-Store} but uses an indexable store as a
      backend (rather than a content-addressable one). *)
  module Store_indexable
      (S : Indexable.S)
      (H : Hash.S with type t = S.hash)
      (C : S with type t = S.value) :
    Store
      with type 'a t = 'a S.t
       and type key = S.key
       and type value = S.value
       and type hash = S.hash
end
