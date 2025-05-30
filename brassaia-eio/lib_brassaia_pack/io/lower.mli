(*
 * Copyright (c) 2023 Tarides <contact@tarides.com>
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
module Io = Io.Unix

type volume_identifier = string [@@deriving brassaia]

module Volume : sig
  type t

  type open_error =
    [ Io.open_error
    | `Closed
    | `Double_close
    | `Corrupted_control_file of string
    | `Unknown_major_pack_version of string ]

  (** [open_volume path] loads the volume at [path] in read-only. *)
  val open_volume : string -> (t, [> open_error]) result

  (** [path t] is the directory that contains the volume. *)
  val path : t -> string

  (** [is_empty t] returns whether [t] is empty or not. *)
  val is_empty : t -> bool

  (** [control t] returns the control file payload for the volume. *)
  val control : t -> Control_file.Payload.Volume.Latest.t option

  (** [identifier t] is a unique idendifier for the volume. *)
  val identifier : t -> volume_identifier
end

type t

type open_error = [Volume.open_error | `Volume_missing of string]

type close_error = [ | Io.close_error]

type add_error =
  [ open_error
  | `Ro_not_allowed
  | `Multiple_empty_volumes
  | `File_exists of string
  | `Invalid_parent_directory ]

(** [open_volumes ~readonly ~volume_num lower_root] loads all volumes located in the
      directory [lower_root].

      [volume_num] is the number of volumes that are expected in [lower_root]. 0
      is valid for an empty lower.

      [Error `Volume_missing path] is returned if an expected volume is not on
      disk, with the path to first volume that is missing. This can happen if
      [volume_num] is larger than the number of volumes on disk, or if one of
      the volume directories has been renamed accidentally.

      If [readonly] is false, no write operations are allowed. *)
val open_volumes :
  readonly:bool -> volume_num:int -> string -> (t, [> open_error]) result

(** [reload ~volume_num t] reloads volumes located in the root directory of
      [t], using [volume_num] as the expected number of volumes. *)
val reload : volume_num:int -> t -> (unit, [> open_error]) result

(** [close t] closes all resources opened by [t]. *)
val close : t -> (unit, [> close_error]) result

(** [volume_num t] returns the number of volumes in the lower [t]. *)
val volume_num : t -> int

(** [add_volume t] adds a new empty volume to [t].

      If there is already an empty volume, [Error `Multiple_empty_volumes] is
      returned. Only one empty volume is allowed.

      If [t] is read-only, [Error `Ro_not_allowed] is returned. *)
val add_volume : t -> (Volume.t, [> add_error]) result

(** [find_volume ~off t] returns the {!Volume} that contains [off]. *)
val find_volume : off:int63 -> t -> Volume.t option

(** [read_exn ~off ~len ~volume t b] will read [len] bytes from a global [off]
      located in the volume with identifier [volume]. The volume identifier of
      the volume where the read occurs is returned.

      If [volume] is not provided, {!find_volume} will be used to attempt to
      locate the correct volume for the read. *)
val read_exn :
  off:int63 ->
  len:int ->
  ?volume:volume_identifier ->
  t ->
  bytes ->
  volume_identifier

(** [set_readonly t flag] changes the writing permission of the lower layer
      (where [true] is read only). This should only be called by the GC worker
      to temporarily allow RW before calling {!archive_seq_exn}. *)
val set_readonly : t -> bool -> unit

(** [archive_seq ~upper_root ~generation ~to_archive t] is called by the GC
      worker during the creation of the new [generation] to archive [to_archive]
      in the lower layer and returns the identifier of the volume where data was
      appended.

      It is the only write operation allowed on the lower layer, and it makes no
      observable change as the control file is left untouched : instead new
      changes are written to volume.gen.control, which is swapped during GC
      finalization. *)
val archive_seq_exn :
  upper_root:string ->
  generation:int ->
  to_archive:(int63 * string Seq.t) list ->
  t ->
  volume_identifier

(** Same as [read_exn] but will read at least [min_len] bytes and at most
      [max_len]. Returns the read length and the volume identifier from which
      the data was fetched. *)
val read_range_exn :
  off:int63 ->
  min_len:int ->
  max_len:int ->
  ?volume:volume_identifier ->
  t ->
  bytes ->
  int * volume_identifier

type create_error :=
  [open_error | close_error | add_error | `Sys_error of string]

(** [create_from ~src ~dead_header_size ~size lower_root] initializes the
      first lower volume in the directory [lower_root] by moving the suffix file
      [src] with end offset [size]. *)
val create_from :
  src:string ->
  dead_header_size:int ->
  size:Int63.t ->
  string ->
  (unit, [> create_error]) result

(** [swap ~volume ~generation ~volume_num t] will rename a new volume control
      file in [volume] for [generation] of GC and then reload the lower with
      [volume_num] volumes. *)
val swap :
  volume:volume_identifier ->
  generation:int ->
  volume_num:int ->
  t ->
  ( unit,
    [> `Volume_not_found of string | `Sys_error of string | open_error] )
  result

(** [cleanup ~generation t] will attempt to cleanup the appendable volume if a
      GC crash has occurred. *)
val cleanup : generation:int -> t -> (unit, [> `Sys_error of string]) result
