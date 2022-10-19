(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021-2022 Tarides <contact@tarides.com>                     *)
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

(** Conversion of a [Stats_trace] to a summary that is both pretty-printable and
    exportable to JSON.

    The main type [t] here isn't versioned like a [Stats_trace.t] is. It is okey
    to break compatibilty with existing JSON files. The important replay results
    are expected to still exist as a stat trace form.

    Computing a summary may take a long time if the input [Stats_trace] is long.
    Count ~1000 commits per second. *)

module Def = Tezos_context_recording.Stats_trace_definition
module Conf = Trace_stats_summary_conf
module Utils = Trace_stats_summary_utils
module Vs = Utils.Variable_summary
module Trace_common = Tezos_context_recording.Trace_common

type initial_row =
  [ `Frequent_op of Def.Frequent_op.tag * Def.Frequent_op.payload
  | `Commit of Def.Commit_op.payload ]

type last_row =
  [ `Close of Def.Stats_op.payload
  | `Commit of Def.Commit_op.payload
  | `Dump_context of Def.Stats_op.payload ]

type gc_event = [`Start | `End of Def.Gc.t]

type block = initial_row list * last_row * gc_event list

(* Section 1/4 - Type of a summary. *)

type curve = Utils.curve [@@deriving repr]

(** A stats trace can be chunked into {e blocks}. A {e blocks} is made of 2
    phases, first the {e buildup} and then the {e commit}.

    The duration of a {e buildup} can be split into 2 parts: 1. the time spend
    in each operation and 2. the sum of the time spent between, before and after
    all operations. The first being the {e seen buildup} and the second the
    {e unseen buildup}.

    The total duration of a block is the sum of the durations of the {e commit},
    the {e seen buildup} and the {e unseen buildup}.

    Where there is a one to one correspondance between commits and blocks, there
    isn't such a correspondence between blocks and block levels. Some blocks are
    said "ophan" if they are dropped by the blockchain. Orphan blocks are often
    included in the stats.

    For simplicity, IF the stats trace ends with a "close" operation, that close
    operation is considered being part of the buildup of the previous block, and
    the time elapsed between the end of the last commit and the beginning on the
    merge is added to the unseen span of the block. *)
module Span = struct
  module Key = struct
    (** The unitary operations played. We recorded the length of all of these. *)
    type atom_seen =
      [`Frequent_op of Def.Frequent_op.tag | `Commit | `Close | `Dump_context]
    [@@deriving repr]

    (** All spans. *)
    type t =
      [ `Frequent_op of Def.Frequent_op.tag
      | `Commit
      | `Close
      | `Dump_context
      | `Unseen
      | `Buildup
      | `Block ]
    [@@deriving repr]

    let all_tree_ops : atom_seen list =
      [
        `Frequent_op (`Tree `Empty);
        `Frequent_op (`Tree `Of_raw);
        `Frequent_op (`Tree `Of_value);
        `Frequent_op (`Tree `Mem);
        `Frequent_op (`Tree `Mem_tree);
        `Frequent_op (`Tree `Find);
        `Frequent_op (`Tree `Is_empty);
        `Frequent_op (`Tree `Kind);
        `Frequent_op (`Tree `Hash);
        `Frequent_op (`Tree `Equal);
        `Frequent_op (`Tree `To_value);
        `Frequent_op (`Tree `Clear);
        `Frequent_op (`Tree `Find_tree);
        `Frequent_op (`Tree `Add);
        `Frequent_op (`Tree `Add_tree);
        `Frequent_op (`Tree `Remove);
        `Frequent_op (`Tree `Fold);
      ]

    let all_frequent_ops : atom_seen list =
      all_tree_ops
      @ [
          `Frequent_op `Find_tree;
          `Frequent_op `Fold;
          `Frequent_op `Add_tree;
          `Frequent_op `Mem;
          `Frequent_op `Mem_tree;
          `Frequent_op `Find;
          `Frequent_op `Get_protocol;
          `Frequent_op `Hash;
          `Frequent_op `Find_predecessor_block_metadata_hash;
          `Frequent_op `Find_predecessor_ops_metadata_hash;
          `Frequent_op `Get_test_chain;
          `Frequent_op `Exists;
          `Frequent_op `Retrieve_commit_info;
          `Frequent_op `Add;
          `Frequent_op `Remove;
          `Frequent_op `Add_protocol;
          `Frequent_op `Add_predecessor_block_metadata_hash;
          `Frequent_op `Add_predecessor_ops_metadata_hash;
          `Frequent_op `Add_test_chain;
          `Frequent_op `Remove_test_chain;
          `Frequent_op `Fork_test_chain;
          `Frequent_op `Checkout;
          `Frequent_op `Clear_test_chain;
          `Frequent_op `Init;
          `Frequent_op `Restore_context;
        ]

    let all_atoms_seen : atom_seen list =
      all_frequent_ops @ [`Commit; `Close; `Dump_context]

    let all_atoms : t list = (all_atoms_seen :> t list) @ [`Unseen]

    let all : t list = all_atoms @ [`Buildup; `Block]

    let to_string : [< t] -> string =
      let to_lower_repr_string t v =
        match String.split_on_char '"' (Irmin.Type.to_string t v) with
        | [""; s; ""] -> s |> String.lowercase_ascii
        | _ -> Fmt.failwith "Could not encode span name to string"
      in
      fun v ->
        let v = (v :> t) in
        match v with
        | `Frequent_op (`Tree v) ->
            "tree." ^ to_lower_repr_string Def.Frequent_op.tree_t v
        | `Frequent_op v -> to_lower_repr_string Def.Frequent_op.tag_t v
        | `Commit -> "commit"
        | `Close -> "close"
        | `Dump_context -> "dump_context"
        | `Unseen -> "unseen"
        | `Buildup -> "buildup"
        | `Block -> "block"

    let of_string : string -> (t, [`Msg of string]) result =
     fun s ->
      match String.split_on_char '.' s with
      | ["commit"] -> Ok `Commit
      | ["close"] -> Ok `Close
      | ["dump_context"] -> Ok `Dump_context
      | ["unseen"] -> Ok `Unseen
      | ["buildup"] -> Ok `Buildup
      | ["block"] -> Ok `Block
      | ["tree"; s] -> (
          let s = "\"" ^ String.capitalize_ascii s ^ "\"" in
          match Irmin.Type.of_string Def.Frequent_op.tree_t s with
          | Ok v -> Ok (`Frequent_op (`Tree v))
          | Error _ as e -> e)
      | [s] -> (
          let s = "\"" ^ String.capitalize_ascii s ^ "\"" in
          match Irmin.Type.of_string Def.Frequent_op.tag_t s with
          | Ok v -> Ok (`Frequent_op v)
          | Error _ as e -> e)
      | _ -> Error (`Msg "Could not decode span key string")
  end

  module Val = struct
    (** Statistics for a given span over the full stat trace.

        The [count] variable is the number of occurences of a span per block and
        [cumu_count] is the cumulative from the beginning.

        The [duration] variable is the length of a span occurence and
        [cumu_duration] is the cumulative from the beginning. The x axis for the
        [evolution] of these [Vs] is the (resampled) number of blocks.

        The [count] for [commit] and [unseen] is trivialy equal to 1. The same
        is almost true for [checkout] too (as the genesis commit has no
        checkout). *)
    type t = {
      count : Vs.t;
      cumu_count : Vs.t;
      duration : Vs.t;
      duration_log_scale : Vs.t;
      cumu_duration : Vs.t;
    }
    [@@deriving repr]
  end

  module Map = struct
    module M = Stdlib.Map.Make (struct
      type t = Key.t

      let compare = compare
    end)

    include M
  end

  type map = Val.t Map.t

  let map_t : map Irmin.Type.t =
    let encode map =
      Map.bindings map |> List.map (fun (k, v) -> (Key.to_string k, v))
    in
    let decode l =
      let key_of_string k =
        match Key.of_string k with
        | Ok k -> k
        | Error (`Msg msg) ->
            Fmt.failwith "Could not convert string back to key: %s" msg
      in
      List.map (fun (k, v) -> (key_of_string k, v)) l
      |> List.to_seq |> Map.of_seq
    in
    Irmin.Type.(map (Json.assoc Val.t) decode encode)
end

type once_per_commit_stat = {value : Vs.t; diff_per_block : Vs.t}
[@@deriving repr]

(** Stats gathered on certain directory sizes within the imin store *)
module Watched_node = struct
  module Key = struct
    type t = Def.watched_node [@@deriving repr]

    let to_string v =
      match String.split_on_char '"' (Irmin.Type.to_string t v) with
      | [""; s; ""] -> s |> String.lowercase_ascii
      | _ -> Fmt.failwith "Could not encode node name to json"

    let of_string s =
      let s = "\"" ^ String.capitalize_ascii s ^ "\"" in
      match Irmin.Type.of_string t s with Ok v -> Ok v | Error _ as e -> e
  end

  module Val = struct
    type t = once_per_commit_stat [@@deriving repr]
  end

  module Map = struct
    module M = Stdlib.Map.Make (struct
      type t = Key.t

      let compare = compare
    end)

    include M
  end

  type map = Val.t Map.t

  let map_t : map Irmin.Type.t =
    let encode map =
      Map.bindings map |> List.map (fun (k, v) -> (Key.to_string k, v))
    in
    let decode l =
      let key_of_string k =
        match Key.of_string k with
        | Ok k -> k
        | Error (`Msg msg) ->
            Fmt.failwith "Could not convert string back to key: %s" msg
      in
      List.map (fun (k, v) -> (key_of_string k, v)) l
      |> List.to_seq |> Map.of_seq
    in
    Irmin.Type.(map (Json.assoc Val.t) decode encode)
end

(** Summary of an entry contained in [Def.bag_of_stat].

    Properties of such a variables:

    - Is sampled before each commit operation.
    - Is sampled after each commit operation.
    - Is sampled in header (but might be nan).
    - Is sampled after close.
    - Most of these entries are counters and grow linearly, it implies that no
      smoothing is necessary for the downsampled curve in these cases, and that
      the histogram is best viewed on a linear scale - as opposed to a log
      scale. The other entries are summarised using
      [~is_linearly_increasing:false].

    The [value_after_commit] is initially fed with the value in the header (i.e.
    the value recorded just before the start of the play). *)
type bag_stat = {
  value_before_commit : Vs.t;
  value_after_commit : Vs.t;
  diff_per_block : Vs.t;
  diff_per_buildup : Vs.t;
  diff_per_commit : Vs.t;
}
[@@deriving repr]

type pack = {
  finds_total : bag_stat;
  finds_from_staging : bag_stat;
  finds_from_lru : bag_stat;
  finds_from_pack_direct : bag_stat;
  finds_from_pack_indexed : bag_stat;
  cache_misses : bag_stat;
  appended_hashes : bag_stat;
  appended_offsets : bag_stat;
  inode_add : bag_stat;
  inode_remove : bag_stat;
  inode_of_seq : bag_stat;
  inode_of_raw : bag_stat;
  inode_rec_add : bag_stat;
  inode_rec_remove : bag_stat;
  inode_to_binv : bag_stat;
  inode_decode_bin : bag_stat;
  inode_encode_bin : bag_stat;
}
[@@deriving repr]

type tree = {
  contents_hash : bag_stat;
  contents_find : bag_stat;
  contents_add : bag_stat;
  contents_mem : bag_stat;
  node_hash : bag_stat;
  node_mem : bag_stat;
  node_index : bag_stat;
  node_add : bag_stat;
  node_find : bag_stat;
  node_val_v : bag_stat;
  node_val_find : bag_stat;
  node_val_list : bag_stat;
}
[@@deriving repr]

type index = {
  bytes_read : bag_stat;
  nb_reads : bag_stat;
  bytes_written : bag_stat;
  nb_writes : bag_stat;
  bytes_both : bag_stat;
  nb_both : bag_stat;
  nb_merge : bag_stat;
  cumu_data_bytes : bag_stat;
  merge_durations : float list;
}
[@@deriving repr]

type ocaml_gc = {
  minor_words : bag_stat;
  promoted_words : bag_stat;
  major_words : bag_stat;
  minor_collections : bag_stat;
  major_collections : bag_stat;
  compactions : bag_stat;
  major_heap_bytes : bag_stat;
  major_heap_top_bytes : bag_stat;
}
[@@deriving repr]

type disk = {
  index_data : bag_stat;
  index_log : bag_stat;
  index_log_async : bag_stat;
  store_dict : bag_stat;
  store_pack : bag_stat;
}
[@@deriving repr]

type store = {watched_nodes : Watched_node.map} [@@deriving repr]

type rusage = {
  utime : bag_stat;
  stime : bag_stat;
  maxrss : bag_stat;
  minflt : bag_stat;
  majflt : bag_stat;
  inblock : bag_stat;
  oublock : bag_stat;
  nvcsw : bag_stat;
  nivcsw : bag_stat;
}
[@@deriving repr]

type block_specs = {
  level_over_blocks : Utils.curve;
  tzop_count : once_per_commit_stat;
  tzop_count_tx : once_per_commit_stat;
  tzop_count_contract : once_per_commit_stat;
  tzgas_used : once_per_commit_stat;
  tzstorage_size : once_per_commit_stat;
  tzcycle_snapshot : once_per_commit_stat;
  tztime : once_per_commit_stat;
  tzsolvetime : once_per_commit_stat;
  ev_count : once_per_commit_stat;
}
[@@deriving repr]

type main_activity = {
  span : Span.map;
  rusage : rusage;
  index : index;
  block_specs : block_specs;
  cpu_usage : Vs.t;
  block_count : int;
}
[@@deriving repr]

type gc = {gc_stats : Def.Gc.t; main_activity : main_activity} [@@deriving repr]

(** The type of a summary *)
type t = {
  summary_timeofday : float;
  summary_hostname : string;
  curves_sample_count : int;
  moving_average_half_life_ratio : float;
  (* Stats from [Def.header]. *)
  header : Def.header;
  timestamp_wall0 : float;
  timestamp_cpu0 : float;
  (* Stats derived from [Def.row]s. *)
  elapsed_wall : float;
  elapsed_wall_over_blocks : Utils.curve;
  elapsed_cpu : float;
  elapsed_cpu_over_blocks : Utils.curve;
  op_count : int;
  span : Span.map;
  block_count : int;
  cpu_usage : Vs.t;
  index : index;
  pack : pack;
  tree : tree;
  ocaml_gc : ocaml_gc;
  disk : disk;
  rusage : rusage;
  block_specs : block_specs;
  store : store;
  gcs : gc list; (* gcs : Def.Gc.t list; *)
}
[@@deriving repr]

(* Section 2/4 - Converters from stats_trace to element of summary. *)
let create_vs block_count =
  Vs.create_acc
    ~distribution_bin_count:Conf.histo_bin_count
    ~out_sample_count:Conf.curves_sample_count
    ~in_period_count:(block_count + 1)
    ~evolution_resampling_mode:`Next_neighbor

let create_vs_exact block_count header_samples =
  let vs = create_vs block_count ~evolution_smoothing:`None ~scale:`Linear in
  Vs.accumulate vs header_samples

let create_vs_smooth block_count header_samples =
  let hlr = Conf.moving_average_half_life_ratio in
  let rt = Conf.moving_average_relevance_threshold in
  let vs =
    create_vs block_count ~evolution_smoothing:(`Ema (hlr, rt)) ~scale:`Linear
  in
  Vs.accumulate vs header_samples

let create_vs_smooth_log block_count header_samples =
  let hlr = Conf.moving_average_half_life_ratio in
  let rt = Conf.moving_average_relevance_threshold in
  let vs =
    create_vs block_count ~evolution_smoothing:(`Ema (hlr, rt)) ~scale:`Log
  in
  Vs.accumulate vs header_samples

(** Accumulator for the [span] field of [t]. *)
module Span_folder = struct
  type span_acc = {
    sum_count : int;
    sum_duration : float;
    count : Vs.acc;
    cumu_count : Vs.acc;
    duration : Vs.acc;
    duration_log_scale : Vs.acc;
    cumu_duration : Vs.acc;
  }

  type acc = {
    per_span : span_acc Span.Map.t;
    seen_atoms_durations_in_block : float list Span.Map.t;
    timestamp_before : float;
    commits_processed : int;
  }

  let create timestamp_before block_count =
    let seen_atoms_durations_in_block0 =
      List.map
        (fun atom_seen -> (atom_seen, []))
        (Span.Key.all_atoms_seen :> Span.Key.t list)
      |> List.to_seq |> Span.Map.of_seq
    in
    let acc0 =
      let acc0_per_span =
        let count = create_vs_smooth block_count [] in
        let cumu_count = create_vs_exact block_count [0.] in
        let duration = create_vs_smooth block_count [] in
        let duration_log_scale = create_vs_smooth_log block_count [] in
        let cumu_duration = create_vs_exact block_count [0.] in
        {
          sum_count = 0;
          sum_duration = 0.;
          count;
          cumu_count;
          duration;
          duration_log_scale;
          cumu_duration;
        }
      in
      let per_span =
        List.map (fun span -> (span, acc0_per_span)) Span.Key.all
        |> List.to_seq |> Span.Map.of_seq
      in
      {
        per_span;
        seen_atoms_durations_in_block = seen_atoms_durations_in_block0;
        timestamp_before;
        commits_processed = 0;
      }
    in

    let on_atom_seen_duration32 acc (span : Span.Key.atom_seen) (d : int32) =
      let d = Int32.float_of_bits d in
      let span = (span :> Span.Key.t) in
      let seen_atoms_durations_in_block =
        let m = acc.seen_atoms_durations_in_block in
        let l = d :: Span.Map.find span m in
        Span.Map.add span l m
      in
      {acc with seen_atoms_durations_in_block}
    in

    let on_durations (span : Span.Key.t) (new_durations : float list) acc =
      let acc' = Span.Map.find span acc.per_span in
      let new_count = List.length new_durations in
      let sum_count = acc'.sum_count + new_count in
      let sum_duration =
        acc'.sum_duration +. List.fold_left ( +. ) 0. new_durations
      in
      let count = Vs.accumulate acc'.count [float_of_int new_count] in
      let cumu_count = Vs.accumulate acc'.cumu_count [float_of_int sum_count] in
      let duration = Vs.accumulate acc'.duration new_durations in
      let duration_log_scale =
        Vs.accumulate acc'.duration_log_scale new_durations
      in
      let cumu_duration = Vs.accumulate acc'.cumu_duration [sum_duration] in
      let acc' =
        {
          sum_count;
          sum_duration;
          count;
          cumu_count;
          duration;
          duration_log_scale;
          cumu_duration;
        }
      in
      {acc with per_span = Span.Map.add span acc' acc.per_span}
    in

    let on_block_end timestamp_after acc =
      let list_one span =
        Span.Map.find span acc.seen_atoms_durations_in_block
      in
      let sum_one span = List.fold_left ( +. ) 0. (list_one span) in
      let sum_several spans =
        let spans = (spans :> Span.Key.t list) in
        List.fold_left (fun cumu span -> cumu +. sum_one span) 0. spans
      in
      let total_duration = timestamp_after -. acc.timestamp_before in
      let acc =
        List.fold_left
          (fun acc tag -> on_durations tag (list_one tag) acc)
          acc
          (Span.Key.all_atoms_seen :> Span.Key.t list)
      in
      let acc =
        acc
        |> on_durations
             `Unseen
             [total_duration -. sum_several Span.Key.all_atoms_seen]
        |> on_durations `Buildup [total_duration -. sum_one `Commit]
        |> on_durations `Block [total_duration]
      in
      {
        acc with
        seen_atoms_durations_in_block = seen_atoms_durations_in_block0;
        timestamp_before = timestamp_after;
        commits_processed = acc.commits_processed + 1;
      }
    in

    let accumulate acc ((rows, last_row, _) : block) =
      let acc =
        List.fold_left
          (fun acc -> function
            | `Commit pl ->
                on_atom_seen_duration32 acc `Commit pl.Def.Commit_op.duration
            | `Frequent_op (tag, pl) ->
                let tag : Span.Key.atom_seen = `Frequent_op tag in
                on_atom_seen_duration32 acc tag pl)
          acc
          rows
      in
      match last_row with
      | `Commit pl ->
          on_atom_seen_duration32 acc `Commit pl.duration
          |> on_block_end pl.Def.Commit_op.after.timestamp_wall
      | `Dump_context pl ->
          on_atom_seen_duration32 acc `Dump_context pl.duration
          |> on_block_end pl.after.timestamp_wall
      | `Close pl ->
          on_atom_seen_duration32 acc `Close pl.duration
          |> on_block_end pl.after.timestamp_wall
    in

    let finalise {per_span; _} =
      Span.Map.map
        (fun acc ->
          {
            Span.Val.count = Vs.finalise acc.count;
            cumu_count = Vs.finalise acc.cumu_count;
            duration = Vs.finalise acc.duration;
            duration_log_scale = Vs.finalise acc.duration_log_scale;
            cumu_duration = Vs.finalise acc.cumu_duration;
          })
        per_span
    in

    Trace_common.Parallel_folders.folder acc0 accumulate finalise
end

(** Summary computation for statistics recorded in [Def.bag_of_stat]. *)
module Bag_stat_folder = struct
  type acc = {
    value_before_commit : Vs.acc;
    value_after_commit : Vs.acc;
    diff_per_block : Vs.acc;
    diff_per_buildup : Vs.acc;
    diff_per_commit : Vs.acc;
    prev_value : float;
    (* constants *)
    value_of_bag : Def.bag_of_stats -> float;
    should_cumulate_value : bool;
  }

  let create_acc ?(is_linearly_increasing = true)
      ?(should_cumulate_value = false) initial_bag block_count value_of_bag =
    let value_just_before = value_of_bag initial_bag in
    let f =
      if is_linearly_increasing then create_vs_exact block_count
      else create_vs_smooth block_count
    in
    let value_before_commit = f [] in
    let value_after_commit =
      (* Consider the header to virtually follow a commit. That way the
         [value_after_commit] stat will contain a point before and after the
         replay. *)
      f [value_just_before]
    in
    let diff_per_block = create_vs_smooth block_count [] in
    let diff_per_buildup = create_vs_smooth block_count [] in
    let diff_per_commit = create_vs_smooth block_count [] in
    {
      value_before_commit;
      value_after_commit;
      diff_per_block;
      diff_per_buildup;
      diff_per_commit;
      prev_value = value_just_before;
      value_of_bag;
      should_cumulate_value;
    }

  let accumulate acc ((_, last_row, _) : block) =
    let before, after =
      match (last_row : last_row) with
      | `Commit {before; after; _} -> (before, after)
      | `Close {before; after; _} | `Dump_context {before; after; _} ->
          (before, after)
    in
    let va = acc.value_of_bag before in
    let vb = acc.value_of_bag after in
    let va, vb =
      if acc.should_cumulate_value then
        (acc.prev_value +. va, acc.prev_value +. va +. vb)
      else (va, vb)
    in
    let diff_block = vb -. acc.prev_value in
    let diff_buildup = va -. acc.prev_value in
    let diff_commit = vb -. va in
    let value_before_commit = Vs.accumulate acc.value_before_commit [va] in
    let value_after_commit = Vs.accumulate acc.value_after_commit [vb] in
    let diff_per_block = Vs.accumulate acc.diff_per_block [diff_block] in
    let diff_per_buildup = Vs.accumulate acc.diff_per_buildup [diff_buildup] in
    let diff_per_commit = Vs.accumulate acc.diff_per_commit [diff_commit] in
    {
      acc with
      value_before_commit;
      value_after_commit;
      diff_per_block;
      diff_per_buildup;
      diff_per_commit;
      prev_value = vb;
    }

  let finalise acc : bag_stat =
    {
      value_before_commit = Vs.finalise acc.value_before_commit;
      value_after_commit = Vs.finalise acc.value_after_commit;
      diff_per_block = Vs.finalise acc.diff_per_block;
      diff_per_buildup = Vs.finalise acc.diff_per_buildup;
      diff_per_commit = Vs.finalise acc.diff_per_commit;
    }

  let create ?should_cumulate_value ?is_linearly_increasing initial_bag
      block_count value_of_bag =
    let acc0 =
      create_acc
        ?should_cumulate_value
        ?is_linearly_increasing
        initial_bag
        block_count
        value_of_bag
    in
    Trace_common.Parallel_folders.folder acc0 accumulate finalise
end

(** Summary computation for statistics recorded once per commit. *)
module Once_per_commit_folder = struct
  type acc = {
    value : Vs.acc;
    diff_per_block : Vs.acc;
    prev_value : float;
    (* constants *)
    value_of_commit : Def.Commit_op.payload option -> float;
    should_cumulate_value : bool;
  }

  let create_acc ?(is_linearly_increasing = true)
      ?(should_cumulate_value = false) block_count ?value_of_header
      value_of_commit =
    let value_in_header =
      match value_of_header with None -> Float.nan | Some x -> x
    in
    let value =
      if is_linearly_increasing then
        create_vs_exact block_count [value_in_header]
      else create_vs_smooth block_count [value_in_header]
    in
    let diff_per_block = create_vs_smooth block_count [] in
    {
      value;
      diff_per_block;
      prev_value = value_in_header;
      value_of_commit;
      should_cumulate_value;
    }

  let accumulate acc ((_, last_row, _) : block) =
    let v =
      match last_row with
      | `Commit (pl : Def.Commit_op.payload) -> acc.value_of_commit (Some pl)
      | `Close _ | `Dump_context _ -> acc.value_of_commit None
    in
    let v = if acc.should_cumulate_value then acc.prev_value +. v else v in
    let diff_block = v -. acc.prev_value in
    let value = Vs.accumulate acc.value [v] in
    let diff_per_block = Vs.accumulate acc.diff_per_block [diff_block] in
    {acc with value; diff_per_block; prev_value = v}

  let finalise acc : once_per_commit_stat =
    {
      value = Vs.finalise acc.value;
      diff_per_block = Vs.finalise acc.diff_per_block;
    }

  let create ?should_cumulate_value ?is_linearly_increasing block_count
      ?value_of_header value_of_commit =
    let acc0 =
      create_acc
        ?should_cumulate_value
        ?is_linearly_increasing
        block_count
        ?value_of_header
        value_of_commit
    in
    Trace_common.Parallel_folders.folder acc0 accumulate finalise
end

let watched_nodes_folder _header block_count =
  let acc0 =
    Stdlib.List.init (List.length Def.watched_nodes) (fun i ->
        Once_per_commit_folder.create_acc block_count (fun pl_opt ->
            match pl_opt with
            | None -> Float.nan
            | Some (pl : Def.Commit_op.payload) ->
                Stdlib.List.nth pl.store_after.watched_nodes_length i))
  in
  let accumulate acc block =
    Stdlib.List.map
      (fun acc_elt -> Once_per_commit_folder.accumulate acc_elt block)
      acc
  in
  let finalise acc =
    Stdlib.List.map2
      (fun tag acc_elt -> (tag, Once_per_commit_folder.finalise acc_elt))
      Def.watched_nodes
      acc
    |> List.to_seq |> Watched_node.Map.of_seq
  in
  Trace_common.Parallel_folders.folder acc0 accumulate finalise

(** Build a resampled curve of timestamps. *)
let elapsed_wall_over_blocks_folder header block_count =
  let open Def in
  let len0 = block_count + 1 in
  let len1 = Conf.curves_sample_count in
  let v0 = header.initial_stats.timestamp_wall in
  let acc0 = Utils.Resample.create_acc `Interpolate ~len0 ~len1 ~v00:v0 in
  let accumulate acc ((_, last_row, _) : block) =
    let after =
      match (last_row : last_row) with
      | `Commit pl -> pl.after
      | `Close pl | `Dump_context pl -> pl.after
    in
    Utils.Resample.accumulate acc after.timestamp_wall
  in

  let finalise acc =
    Utils.Resample.finalise acc |> List.map (fun t -> t -. v0)
  in
  Trace_common.Parallel_folders.folder acc0 accumulate finalise

(** Build a resampled curve of timestamps. *)
let elapsed_cpu_over_blocks_folder header block_count =
  let open Def in
  let len0 = block_count + 1 in
  let len1 = Conf.curves_sample_count in
  let v0 = header.initial_stats.timestamp_cpu in
  let acc0 = Utils.Resample.create_acc `Interpolate ~len0 ~len1 ~v00:v0 in
  let accumulate acc ((_, last_row, _) : block) =
    let after =
      match (last_row : last_row) with
      | `Commit pl -> pl.after
      | `Close pl | `Dump_context pl -> pl.after
    in
    Utils.Resample.accumulate acc after.timestamp_cpu
  in
  let finalise acc =
    Utils.Resample.finalise acc |> List.map (fun t -> t -. v0)
  in
  Trace_common.Parallel_folders.folder acc0 accumulate finalise

(** Build a list of all the merge durations. *)
let merge_durations_folder =
  let acc0 = [] in
  let accumulate l ((_, last_row, _) : block) =
    let before, after =
      match (last_row : last_row) with
      | `Commit {before; after; _} -> (before, after)
      | `Close {before; after; _} | `Dump_context {before; after; _} ->
          (before, after)
    in
    let l = List.rev_append before.index.new_merge_durations l in
    let l = List.rev_append after.index.new_merge_durations l in
    l
  in
  let finalise = List.rev in
  Trace_common.Parallel_folders.folder acc0 accumulate finalise

(** Build a resampled curve of block levels. All the values are nan if the block
    levels are missing from the stat trace. *)
let level_over_blocks_folder block_level_before block_count =
  let len0 = block_count + 1 in
  let len1 = Conf.curves_sample_count in
  let v0 = block_level_before in
  let acc0 = Utils.Resample.create_acc `Interpolate ~len0 ~len1 ~v00:v0 in
  let accumulate acc ((initial_rows, last_row, _) : block) =
    let level =
      match (last_row : last_row) with
      | `Commit pl -> (
          match pl.specs with
          | None -> Float.nan
          | Some specs -> float_of_int specs.level)
      | `Dump_context _ -> Float.nan
      | `Close _ ->
          initial_rows
          |> List.find_map (function
                 | `Frequent_op _ -> None
                 | `Commit Def.Commit_op.{specs = Some {level; _}; _} ->
                     Some (float_of_int level)
                 | `Commit Def.Commit_op.{specs = None; _} -> None)
          |> Option.value ~default:Float.nan
    in
    Utils.Resample.accumulate acc level
  in
  let finalise = Utils.Resample.finalise in
  Trace_common.Parallel_folders.folder acc0 accumulate finalise

let cpu_usage_folder initial_bag block_count =
  let acc0 =
    let vs = create_vs_smooth block_count [] in
    ( initial_bag.Def.timestamp_wall,
      initial_bag.Def.timestamp_cpu,
      vs )
  in
  let accumulate (prev_wall, prev_cpu, vs) ((_, last_row, _) : block) =
    let after =
      match (last_row : last_row) with
      | `Commit pl -> pl.after
      | `Close pl | `Dump_context pl -> pl.after
    in
    let span_wall = after.timestamp_wall -. prev_wall in
    let span_cpu = after.timestamp_cpu -. prev_cpu in
    ( after.timestamp_wall,
      after.timestamp_cpu,
      Vs.accumulate vs [span_cpu /. span_wall] )
  in
  let finalise (_, _, vs) = Vs.finalise vs in
  Trace_common.Parallel_folders.folder acc0 accumulate finalise

(** Substract the first and the last timestamps and count the number of span. *)
let misc_stats_folder header =
  let open Def in
  let acc0 = (0., 0., 0) in
  let accumulate (_, _, count) ((rows, last_row, _) : block) =
    let count =
      List.fold_left
        (fun count _ ->
          if (count + 1) mod 500_000 = 0 then
            Fmt.epr "Seeing op idx=%#d ops\n%!" (count + 1) ;
          count + 1)
        count
        rows
    in
    let after =
      match (last_row : last_row) with
      | `Commit pl -> pl.after
      | `Close pl | `Dump_context pl -> pl.after
    in
    (after.timestamp_wall, after.timestamp_cpu, count + 1)
  in
  let finalise (t, t', count) =
    ( t -. header.initial_stats.timestamp_wall,
      t' -. header.initial_stats.timestamp_cpu,
      count )
  in
  Trace_common.Parallel_folders.folder acc0 accumulate finalise

let bs_folder_of_bag_getter initial_bag block_count ?should_cumulate_value
    ?is_linearly_increasing value_of_bag =
  Bag_stat_folder.create
    ?should_cumulate_value
    ?is_linearly_increasing
    initial_bag
    block_count
    value_of_bag

let pack_folder header block_count =
  let construct finds_total finds_from_staging finds_from_lru
      finds_from_pack_direct finds_from_pack_indexed cache_misses
      appended_hashes appended_offsets inode_add inode_remove inode_of_seq
      inode_of_raw inode_rec_add inode_rec_remove inode_to_binv inode_decode_bin
      inode_encode_bin =
    {
      finds_total;
      finds_from_staging;
      finds_from_lru;
      finds_from_pack_direct;
      finds_from_pack_indexed;
      cache_misses;
      appended_hashes;
      appended_offsets;
      inode_add;
      inode_remove;
      inode_of_seq;
      inode_of_raw;
      inode_rec_add;
      inode_rec_remove;
      inode_to_binv;
      inode_decode_bin;
      inode_encode_bin;
    }
  in

  let acc0 =
    let open Trace_common.Parallel_folders in
    let ofi = float_of_int in
    let x = bs_folder_of_bag_getter header.Def.initial_stats block_count in
    open_ construct
    |+ x (fun bag -> ofi bag.Def.pack.finds_total)
    |+ x (fun bag -> ofi bag.Def.pack.finds_from_staging)
    |+ x (fun bag -> ofi bag.Def.pack.finds_from_lru)
    |+ x (fun bag -> ofi bag.Def.pack.finds_from_pack_direct)
    |+ x (fun bag -> ofi bag.Def.pack.finds_from_pack_indexed)
    |+ x (fun bag -> ofi bag.Def.pack.cache_misses)
    |+ x (fun bag -> ofi bag.Def.pack.appended_hashes)
    |+ x (fun bag -> ofi bag.Def.pack.appended_offsets)
    |+ x (fun bag -> ofi bag.Def.pack.inode_add)
    |+ x (fun bag -> ofi bag.Def.pack.inode_remove)
    |+ x (fun bag -> ofi bag.Def.pack.inode_of_seq)
    |+ x (fun bag -> ofi bag.Def.pack.inode_of_raw)
    |+ x (fun bag -> ofi bag.Def.pack.inode_rec_add)
    |+ x (fun bag -> ofi bag.Def.pack.inode_rec_remove)
    |+ x (fun bag -> ofi bag.Def.pack.inode_to_binv)
    |+ x (fun bag -> ofi bag.Def.pack.inode_decode_bin)
    |+ x (fun bag -> ofi bag.Def.pack.inode_encode_bin)
    |> seal
  in
  Trace_common.Parallel_folders.folder
    acc0
    Trace_common.Parallel_folders.accumulate
    Trace_common.Parallel_folders.finalise

let tree_folder header block_count =
  let construct contents_hash contents_find contents_add contents_mem node_hash
      node_mem node_index node_add node_find node_val_v node_val_find
      node_val_list =
    {
      contents_hash;
      contents_find;
      contents_add;
      contents_mem;
      node_hash;
      node_mem;
      node_index;
      node_add;
      node_find;
      node_val_v;
      node_val_find;
      node_val_list;
    }
  in
  let acc0 =
    let open Trace_common.Parallel_folders in
    let ofi = float_of_int in
    let x = bs_folder_of_bag_getter header.Def.initial_stats block_count in
    open_ construct
    |+ x (fun bag -> ofi bag.Def.tree.contents_hash)
    |+ x (fun bag -> ofi bag.Def.tree.contents_find)
    |+ x (fun bag -> ofi bag.Def.tree.contents_add)
    |+ x (fun bag -> ofi bag.Def.tree.contents_mem)
    |+ x (fun bag -> ofi bag.Def.tree.node_hash)
    |+ x (fun bag -> ofi bag.Def.tree.node_mem)
    |+ x (fun bag -> ofi bag.Def.tree.node_index)
    |+ x (fun bag -> ofi bag.Def.tree.node_add)
    |+ x (fun bag -> ofi bag.Def.tree.node_find)
    |+ x (fun bag -> ofi bag.Def.tree.node_val_v)
    |+ x (fun bag -> ofi bag.Def.tree.node_val_find)
    |+ x (fun bag -> ofi bag.Def.tree.node_val_list)
    |> seal
  in
  Trace_common.Parallel_folders.folder
    acc0
    Trace_common.Parallel_folders.accumulate
    Trace_common.Parallel_folders.finalise

let index_folder initial_bag block_count =
  let construct bytes_read nb_reads bytes_written nb_writes bytes_both nb_both
      nb_merge cumu_data_bytes merge_durations =
    {
      bytes_read;
      nb_reads;
      bytes_written;
      nb_writes;
      bytes_both;
      nb_both;
      nb_merge;
      cumu_data_bytes;
      merge_durations;
    }
  in
  let acc0 =
    let open Trace_common.Parallel_folders in
    let ofi = float_of_int in
    let x = bs_folder_of_bag_getter initial_bag block_count in
    open_ construct
    |+ x (fun bag -> ofi bag.Def.index.bytes_read)
    |+ x (fun bag -> ofi bag.Def.index.nb_reads)
    |+ x (fun bag -> ofi bag.Def.index.bytes_written)
    |+ x (fun bag -> ofi bag.Def.index.nb_writes)
    |+ x (fun bag ->
           ofi (bag.Def.index.bytes_read + bag.Def.index.bytes_written))
    |+ x (fun bag -> ofi (bag.Def.index.nb_reads + bag.Def.index.nb_writes))
    |+ x (fun bag -> ofi bag.Def.index.nb_merge)
    |+ x ~should_cumulate_value:true (fun bag ->
           (* When 1 merge occured, [data_size] bytes were written.

              When 2 merge occured, [data_size * 2 - log_size] bytes were
              written. But here we just count [data_size * 2]. *)
           let merge_count =
             List.length bag.Def.index.new_merge_durations |> Int64.of_int
           in
           let data_size = bag.Def.disk.index_data in
           Int64.to_float (Int64.mul merge_count data_size))
    |+ merge_durations_folder |> seal
  in
  Trace_common.Parallel_folders.folder
    acc0
    Trace_common.Parallel_folders.accumulate
    Trace_common.Parallel_folders.finalise

let ocaml_gc_folder header block_count =
  let construct minor_words promoted_words major_words minor_collections
      major_collections compactions major_heap_bytes major_heap_top_bytes =
    {
      minor_words;
      promoted_words;
      major_words;
      minor_collections;
      major_collections;
      compactions;
      major_heap_bytes;
      major_heap_top_bytes;
    }
  in
  let acc0 =
    let open Trace_common.Parallel_folders in
    let ofi = float_of_int in
    let x = bs_folder_of_bag_getter header.Def.initial_stats block_count in
    let ws = header.Def.word_size / 8 |> float_of_int in
    open_ construct
    |+ x (fun bag -> bag.Def.ocaml_gc.minor_words)
    |+ x (fun bag -> bag.Def.ocaml_gc.promoted_words)
    |+ x (fun bag -> bag.Def.ocaml_gc.major_words)
    |+ x (fun bag -> ofi bag.Def.ocaml_gc.minor_collections)
    |+ x (fun bag -> ofi bag.Def.ocaml_gc.major_collections)
    |+ x (fun bag -> ofi bag.Def.ocaml_gc.compactions)
    |+ x ~is_linearly_increasing:false (fun bag ->
           ofi bag.Def.ocaml_gc.heap_words *. ws)
    |+ x (fun bag -> ofi bag.Def.ocaml_gc.top_heap_words *. ws)
    |> seal
  in
  Trace_common.Parallel_folders.folder
    acc0
    Trace_common.Parallel_folders.accumulate
    Trace_common.Parallel_folders.finalise

let disk_folder header block_count =
  let construct index_data index_log index_log_async store_dict store_pack =
    {index_data; index_log; index_log_async; store_dict; store_pack}
  in
  let acc0 =
    let open Trace_common.Parallel_folders in
    let ofi64 = Int64.to_float in
    let x = bs_folder_of_bag_getter header.Def.initial_stats block_count in
    open_ construct
    |+ x (fun bag -> ofi64 bag.Def.disk.index_data)
    |+ x ~is_linearly_increasing:false (fun bag -> ofi64 bag.Def.disk.index_log)
    |+ x ~is_linearly_increasing:false (fun bag ->
           ofi64 bag.Def.disk.index_log_async)
    |+ x (fun bag -> ofi64 bag.Def.disk.store_dict)
    |+ x (fun bag ->
           (* This would not be linearly increasing with irmin layers *)
           ofi64 bag.Def.disk.store_pack)
    |> seal
  in
  Trace_common.Parallel_folders.folder
    acc0
    Trace_common.Parallel_folders.accumulate
    Trace_common.Parallel_folders.finalise

let rusage_folder initial_bag block_count =
  let construct utime stime maxrss minflt majflt inblock oublock nvcsw nivcsw =
    {utime; stime; maxrss; minflt; majflt; inblock; oublock; nvcsw; nivcsw}
  in
  let acc0 =
    let open Trace_common.Parallel_folders in
    let ofi64 = Int64.to_float in
    let x = bs_folder_of_bag_getter initial_bag block_count in
    open_ construct
    |+ x (fun bag -> bag.Def.rusage.utime)
    |+ x (fun bag -> bag.Def.rusage.stime)
    |+ x (fun bag -> ofi64 bag.Def.rusage.maxrss)
    |+ x (fun bag -> ofi64 bag.Def.rusage.minflt)
    |+ x (fun bag -> ofi64 bag.Def.rusage.majflt)
    |+ x (fun bag -> ofi64 bag.Def.rusage.inblock)
    |+ x (fun bag -> ofi64 bag.Def.rusage.oublock)
    |+ x (fun bag -> ofi64 bag.Def.rusage.nvcsw)
    |+ x (fun bag -> ofi64 bag.Def.rusage.nivcsw)
    |> seal
  in
  Trace_common.Parallel_folders.folder
    acc0
    Trace_common.Parallel_folders.accumulate
    Trace_common.Parallel_folders.finalise

let block_specs_folder block_level_before block_count =
  let construct level_over_blocks tzop_count tzop_count_tx tzop_count_contract
      tzgas_used tzstorage_size tzcycle_snapshot tztime tzsolvetime ev_count =
    {
      level_over_blocks;
      tzop_count;
      tzop_count_tx;
      tzop_count_contract;
      tzgas_used;
      tzstorage_size;
      tzcycle_snapshot;
      tztime;
      tzsolvetime;
      ev_count;
    }
  in
  let acc0 =
    let open Trace_common.Parallel_folders in
    let f of_specs =
      Once_per_commit_folder.create
        ~should_cumulate_value:true
        block_count
        ~value_of_header:0.
        (fun pl_opt ->
          match pl_opt with
          | Some Def.Commit_op.{specs = Some specs; _} ->
              float_of_int (of_specs specs)
          | _ -> Float.nan)
    in
    open_ construct
    |+ level_over_blocks_folder block_level_before block_count
    |+ f (fun specs -> specs.tzop_count)
    |+ f (fun specs -> specs.tzop_count_tx)
    |+ f (fun specs -> specs.tzop_count_contract)
    |+ f (fun specs -> specs.tz_gas_used)
    |+ f (fun specs -> specs.tz_storage_size)
    |+ f (fun specs -> specs.tz_cycle_snapshot)
    |+ f (fun specs -> specs.tz_time)
    |+ f (fun specs -> specs.tz_solvetime)
    |+ f (fun specs -> specs.ev_count)
    |> seal
  in
  Trace_common.Parallel_folders.folder
    acc0
    Trace_common.Parallel_folders.accumulate
    Trace_common.Parallel_folders.finalise

let store_folder header block_count =
  let construct watched_nodes = {watched_nodes} in
  let acc0 =
    let open Trace_common.Parallel_folders in
    open_ construct |+ watched_nodes_folder header block_count |> seal
  in
  Trace_common.Parallel_folders.folder
    acc0
    Trace_common.Parallel_folders.accumulate
    Trace_common.Parallel_folders.finalise

module Gcs_folder = struct
  type main_acc = {initial_bag : Def.bag_of_stats; rev_blocks : block list}

  type acc = {
    state : [`Looking_for_start | `Looking_for_end of main_acc];
    rev_gcs : gc list;
  }

  let compute_main_activity main_acc =
    let blocks = List.rev main_acc.rev_blocks in
    let bag = main_acc.initial_bag in
    let block_count = List.length blocks in
    assert (block_count > 0) ;
    let block_level_before =
      match blocks with
      | [] -> assert false
      | (_, `Commit {specs = Some specs; _}, _) :: _ ->
          specs.level - 1 |> float_of_int
      | _ -> Float.nan
    in
    let construct span rusage index block_specs cpu_usage =
      {span; rusage; index; block_specs; block_count; cpu_usage}
    in
    let pf0 =
      let open Trace_common.Parallel_folders in
      open_ construct
      |+ Span_folder.create bag.timestamp_wall block_count
      |+ rusage_folder bag block_count
      |+ index_folder bag block_count
      |+ block_specs_folder block_level_before block_count
      |+ cpu_usage_folder bag  block_count
      |> seal
    in
    List.fold_left Trace_common.Parallel_folders.accumulate pf0 blocks
    |> Trace_common.Parallel_folders.finalise

  let on_gc_event ((_, last_op, _) : block) acc ev =
    match (acc.state, ev) with
    | `Looking_for_start, `Start ->
        let initial_bag =
          Fmt.epr "Gc `Start\n%!" ;
          match last_op with
          | `Commit pl -> pl.after
          | `Close pl -> pl.after
          | `Dump_context _ -> assert false
        in
        let main_acc = {initial_bag; rev_blocks = []} in
        {acc with state = `Looking_for_end main_acc}
    | `Looking_for_end main_acc, `End gc_stats ->
        let main_activity = compute_main_activity main_acc in
        let s = {gc_stats; main_activity} in
        {rev_gcs = s :: acc.rev_gcs; state = `Looking_for_start}
    | _ -> assert false

  let accumulate acc ((_, _, gc_events) as block : block) =
    let acc =
      match acc.state with
      | `Looking_for_start -> acc
      | `Looking_for_end main_acc ->
          let state =
            `Looking_for_end
              {main_acc with rev_blocks = block :: main_acc.rev_blocks}
          in
          {acc with state}
    in
    List.fold_left (on_gc_event block) acc gc_events

  let finalise acc = List.rev acc.rev_gcs

  let create _header _block_count =
    let acc0 = {state = `Looking_for_start; rev_gcs = []} in
    Trace_common.Parallel_folders.folder acc0 accumulate finalise
end

(* Section 3/4 - Converter from stats_trace to summary *)

(** Fold over [row_seq] and produce the summary.

    {3 Parallel Folders}

    Almost all entries in [t] require to independently fold over the rows of the
    stats trace, but we want:

    - not to fully load the trace in memory,
    - not to reread the trace from disk once for each entry,
    - this current file to be verbose and simple,
    - to have fun with GADTs and avoid mutability.

    All the boilerplate is hidden behind [Trace_common.Parallel_folders], a
    datastructure that holds all folder functions, takes care of feeding the
    rows to those folders, and preseves the types.

    In the code below, [pf0] is the initial parallel folder, before the first
    accumulation. Each [|+ ...] statement declares a [acc, accumulate, finalise]
    triplet, i.e. a folder.

    [val acc : acc] is the initial empty accumulation of a folder.

    [val accumulate : acc -> row -> acc] needs to be folded over all rows of the
    stats trace. Calling [Parallel_folders.accumulate pf row] will feed [row] to
    every folders.

    [val finalise : acc -> v] has to be applied on the final [acc] of a folder
    in order to produce the final value of that folder - which value is meant to
    be stored in [Trace_stats_summary.t]. Calling [Parallel_folders.finalise pf]
    will finalise all folders and pass their result to [construct]. *)
let summarise' header block_count (block_seq : block Seq.t) =
  let block_level_before =
    match header.Def.config.setup with
    | `Play _ -> Float.nan
    | `Replay {initial_block_level = Some v; _} -> float_of_int v
    | `Replay {initial_block_level = None; _} -> 0.
  in
  let construct (elapsed_wall, elapsed_cpu, op_count) elapsed_wall_over_blocks
      elapsed_cpu_over_blocks span cpu_usage_variable pack tree index ocaml_gc
      disk rusage block_specs store gcs =
    {
      summary_hostname = Unix.gethostname ();
      summary_timeofday = Unix.gettimeofday ();
      elapsed_wall;
      elapsed_cpu;
      op_count;
      block_count;
      curves_sample_count = Conf.curves_sample_count;
      moving_average_half_life_ratio = Conf.moving_average_half_life_ratio;
      header;
      timestamp_wall0 = header.initial_stats.timestamp_wall;
      timestamp_cpu0 = header.initial_stats.timestamp_cpu;
      elapsed_wall_over_blocks;
      elapsed_cpu_over_blocks;
      span;
      pack;
      tree;
      cpu_usage = cpu_usage_variable;
      index;
      ocaml_gc;
      disk;
      rusage;
      block_specs;
      store;
      gcs;
    }
  in
  let pf0 =
    let open Trace_common.Parallel_folders in
    open_ construct |+ misc_stats_folder header
    |+ elapsed_wall_over_blocks_folder header block_count
    |+ elapsed_cpu_over_blocks_folder header block_count
    |+ Span_folder.create header.initial_stats.timestamp_wall block_count
    |+ cpu_usage_folder header.initial_stats block_count
    |+ pack_folder header block_count
    |+ tree_folder header block_count
    |+ index_folder header.initial_stats block_count
    |+ ocaml_gc_folder header block_count
    |+ disk_folder header block_count
    |+ rusage_folder header.initial_stats block_count
    |+ block_specs_folder block_level_before block_count
    |+ store_folder header block_count
    |+ Gcs_folder.create header block_count
    |> seal
  in
  Seq.fold_left Trace_common.Parallel_folders.accumulate pf0 block_seq
  |> Trace_common.Parallel_folders.finalise

(** Groups the trace's rows into blocks. *)
let block_seq_when_node_run ~ends_with_close ~block_count row_seq : block Seq.t
    =
  let commit_count = ref 0 in
  let rec aux l ~gc_events (row_seq : _ Seq.t) =
    let saw_all_commits = !commit_count = block_count in
    match (ends_with_close, saw_all_commits) with
    | true, true -> (
        match row_seq () with
        | Seq.Cons (`Gc_finalised stats, rest) ->
            let gc_events = `End stats :: gc_events in
            aux l ~gc_events rest
        | Seq.Cons ((`Close _ as op), _) ->
            Seq.Cons ((List.rev l, op, gc_events), Seq.empty)
        | _ -> assert false)
    | false, true -> Seq.Nil
    | (true | false), false -> (
        match row_seq () with
        | Seq.Nil -> assert false
        | Seq.Cons (op, rest) -> (
            match op with
            | `Dump_context _ -> assert false
            | `Close _ -> assert false
            | `Commit _ as op ->
                incr commit_count ;
                Seq.Cons
                  ( (List.rev l, op, gc_events),
                    fun () -> aux [] ~gc_events:[] rest )
            | `Frequent_op _ as op -> aux (op :: l) ~gc_events rest
            | `Gc_start ->
                let gc_events = `Start :: gc_events in
                aux l ~gc_events rest
            | `Gc_finalised stats ->
                let gc_events = `End stats :: gc_events in
                aux l ~gc_events rest))
  in
  fun () -> aux [] ~gc_events:[] row_seq

(** Groups the trace's rows into blocks, when this trace corresponds to a
    snapshot export *)
let block_seq_when_snapshot_export row_seq : block Seq.t =
  let rec aux row_seq =
    match row_seq () with
    | Seq.Nil -> Fmt.failwith "could not find Dump_context"
    | Seq.Cons ((`Dump_context _ as row), _) ->
        Seq.Cons (([], row, []), Seq.empty)
    | Seq.Cons (_, rest) -> aux rest
  in
  fun () -> aux row_seq

(** Turn a stats trace into a summary.

    The number of blocks to consider may be provided in order to truncate the
    summary. *)
let summarise ?info trace_stats_path =
  ignore info ;
  (* let info = Some (1, false) in *)
  let is_snapshot_export, block_count, ends_with_close =
    match info with
    | Some (is_snapshot_export, block_count, ends_with_close) ->
        (is_snapshot_export, block_count, ends_with_close)
    | None ->
        (* The trace has to be iterated a first time. *)
        Def.open_reader trace_stats_path
        |> (fun (_, _, x) -> x)
        |> Seq.fold_left
             (fun ((is_snapshot_export, commit_count, has_close) as acc) op ->
               (* Fmt.epr "%a\n%!" (Repr.pp Def.row_t) op ; *)
               if has_close then acc
               else if is_snapshot_export then acc
               else
                 match op with
                 | `Commit _ -> (false, commit_count + 1, false)
                 | `Dump_context _ -> (true, 1, false)
                 | _ -> acc)
             (false, 0, false)
  in
  if is_snapshot_export then ()
  else if block_count <= 0 then
    invalid_arg "Can't summarise an empty stats trace" ;
  let _, header, row_seq = Def.open_reader trace_stats_path in

  let block_seq =
    if is_snapshot_export then block_seq_when_snapshot_export row_seq
    else block_seq_when_node_run ~ends_with_close ~block_count row_seq
  in
  summarise' header block_count block_seq

(* Section 4/4 - Conversion from summary to json file *)

let save_to_json v path =
  let j = Fmt.str "%a\n" (Irmin.Type.pp_json t) v in
  let chan = open_out path in
  output_string chan j ;
  Logs.info (fun l -> l "Summary saved to %s" path) ;
  close_out chan ;
  Unix.chmod path 0o444
