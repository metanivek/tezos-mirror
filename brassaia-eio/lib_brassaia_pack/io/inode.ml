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
module Brassaia_pack = Brassaia_eio_pack.Brassaia_pack
include Brassaia_pack.Inode
include Inode_intf

module Make_persistent
    (H : Brassaia.Hash.S)
    (Node : Brassaia.Node.Generic_key.S
              with type hash = H.t
               and type contents_key = H.t Pack_key.t
               and type node_key = H.t Pack_key.t)
    (Inter : Internal with type hash = H.t and type key = H.t Pack_key.t)
    (Pack : Pack_store.S
              with type hash = H.t
               and type key = H.t Pack_key.t
               and type value = Inter.Raw.t) =
struct
  module Raw = Inter.Raw
  module Pack = Pack

  type file_manager = Pack.file_manager

  type dict = Pack.dict

  type dispatcher = Pack.dispatcher

  let to_snapshot = Inter.to_snapshot

  module XKey = Pack_key.Make (H)
  include Make (H) (XKey) (Node) (Inter) (Pack)
  module Snapshot = Inter.Snapshot

  let of_snapshot t ~index v =
    let find ~expected_depth:_ k =
      let v = Pack.unsafe_find ~check_integrity:true t k in
      v
    in
    Inter.Val.of_snapshot ~index v find

  let init = Pack.init

  let integrity_check = Pack.integrity_check

  let purge_lru = Pack.purge_lru

  let key_of_offset = Pack.key_of_offset

  let unsafe_find_no_prefetch t key =
    match Pack.unsafe_find_no_prefetch t key with
    | None -> None
    | Some v ->
        let find ~expected_depth:_ k =
          (* TODO: Remove this dead code. Can the GC traverse `Raw` values? *)
          Pack.unsafe_find ~check_integrity:false t k
        in
        let v = Inter.Val.of_raw find v in
        Some v

  let get_offset t k = Pack.get_offset t k

  let get_length t k = Pack.get_length t k
end
