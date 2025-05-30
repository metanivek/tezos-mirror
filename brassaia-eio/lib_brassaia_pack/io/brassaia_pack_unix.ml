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

(** {1 Store} *)

module Brassaia = Brassaia_eio.Brassaia
module Brassaia_pack = Brassaia_eio_pack.Brassaia_pack

module type S = Store_intf.S

module Maker (Config : Brassaia_pack.Conf.S) =
  Store.Maker (Brassaia_index_unix.Index_unix.Private.Platform) (Async.Unix)
    (Config)

module KV (Config : Brassaia_pack.Conf.S) = struct
  type endpoint = unit

  type hash = Brassaia.Schema.default_hash

  include Pack_key.Store_spec
  module Maker =
    Store.Maker (Brassaia_index_unix.Index_unix.Private.Platform) (Async.Unix)
      (Config)
  module Make (C : Brassaia.Contents.S) = Maker.Make (Brassaia.Schema.KV (C))
end

(** {1 Key and Values} *)

module Pack_key = Pack_key
module Pack_value = Pack_value

(** {1 Integrity Checks} *)

module Checks = Checks

(** {1 Internal} *)

module Stats = Stats

module Index = struct
  module type S = Brassaia_index.Index.S

  module Make (K : Brassaia.Hash.S) = Pack_index.Make_io (K)
end

module Inode = Inode
module Pack_store = Pack_store
module Atomic_write = Atomic_write
module Dict = Dict
module Dispatcher = Dispatcher
module Io = Io
module Async = Async
module Errors = Errors
module Io_errors = Io_errors
module Control_file = Control_file
module Control_file_intf = Control_file_intf
module Append_only_file = Append_only_file
module Chunked_suffix = Chunked_suffix
module Ranges = Ranges
module Sparse_file = Sparse_file
module File_manager = File_manager
module Lower = Lower
module Utils = Utils
module Lru = Lru
module Gc_raw = Gc
module Traverse_pack_file = Traverse_pack_file
module Snapshot = Snapshot
