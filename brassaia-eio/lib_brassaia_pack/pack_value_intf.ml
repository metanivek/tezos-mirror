(*
 * Copyright (c) 2018-2022 Tarides <contact@tarides.com>
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
module Brassaia = Brassaia_eio.Brassaia
open! Import

type length_header = [`Varint] option

type weight = Immediate of int | Deferred of (unit -> int)

type kinded = ..

module type S = sig
  include Brassaia.Type.S

  type hash

  type key

  type kind

  val hash : t -> hash

  val kind : t -> kind

  (** Describes the length header formats for the {i data} sections of pack
      entries. *)
  val length_header : kind -> length_header

  (** [weight t] is the [t]'s LRU weight. *)
  val weight : t -> weight

  val encode_bin :
    dict:(string -> int option) ->
    offset_of_key:(key -> int63 option) ->
    hash ->
    t Brassaia.Type.encode_bin

  val decode_bin :
    dict:(int -> string option) ->
    key_of_offset:(int63 -> key) ->
    key_of_hash:(hash -> key) ->
    t Brassaia.Type.decode_bin

  val decode_bin_length : string -> int -> int

  (** [to_kinded t] returns a {!type-kinded} version of [t]. *)
  val to_kinded : t -> kinded

  (** [of_kinded k] is the inverse of [to_kinded t].

      It is expected that an implementation only works for [k] that is returned
      from [to_kinded t] and will raise an exception otherwise. *)
  val of_kinded : kinded -> t

  (** [encoding] is the data_encoding for {!type-t}. *)
  val encoding : t Data_encoding.t
end

module type T = sig
  type t
end

(* A subset of [Brassaia_pack.Conf.S] relevant to the format of pack entries,
   copied here to avoid cyclic dependencies. *)
module type Config = sig
  val contents_length_header : length_header
end

module type Sigs = sig
  module Kind : sig
    type t =
      | Commit_v1
      | Commit_v2
      | Contents
      | Inode_v1_unstable
      | Inode_v1_stable
      | Inode_v2_root
      | Inode_v2_nonroot
      | Dangling_parent_commit
    [@@deriving brassaia]

    val all : t list

    val to_enum : t -> int

    val to_magic : t -> char

    val of_magic_exn : char -> t

    val pp : t Fmt.t

    (** Raises an exception on [Contents], as the availability of a length
        header is user defined. *)
    val length_header_exn : t -> length_header
  end

  type nonrec weight = weight = Immediate of int | Deferred of (unit -> int)

  (** [kinded] is an extenisble variant that each {!S} extends so that it can
      define {!S.to_kinded} and {!S.of_kinded}. Its purpose is to allow
      containers, such as {!Brassaia_pack_unix.Lru}, to store and return all types
      of {!S} and thus be usable by modules defined over {!S}, such as
      {!Brassaia_pack_unix.Pack_store}. *)
  type nonrec kinded = kinded = ..

  module type S = S with type kind := Kind.t

  module type Config = Config

  module Of_contents
      (_ : Config)
      (Hash : Brassaia.Hash.S)
      (Key : T)
      (Contents : Brassaia.Contents.S) :
    S with type t = Contents.t and type hash = Hash.t and type key = Key.t

  module Of_commit
      (Hash : Brassaia.Hash.S)
      (Key : Brassaia.Key.S with type hash = Hash.t)
      (Commit : Brassaia.Commit.Generic_key.S
                  with type node_key = Key.t
                   and type commit_key = Key.t) : sig
    include S with type t = Commit.t and type hash = Hash.t and type key = Key.t

    module Commit_direct : sig
      type address = Offset of int63 | Hash of hash [@@deriving brassaia]

      type t = {
        node_offset : address;
        parent_offsets : address list;
        info : Commit.Info.t;
      }
      [@@deriving brassaia]
    end
  end
end
