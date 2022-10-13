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

open Trace_stats_summary
module Utils = Trace_stats_summary_utils
module Hashtbl = Stdlib.Hashtbl
module List = Stdlib.List
module Int63 = Optint.Int63

module Pb = struct
  include PrintBox

  (* Some utilities to work with lists instead of array *)

  let transpose_matrix l =
    l |> List.map Array.of_list |> Array.of_list |> PrintBox.transpose
    |> Array.to_list |> List.map Array.to_list

  let matrix_to_text m = List.map (List.map PrintBox.text) m

  let align_matrix where = List.map (List.map (PrintBox.align ~h:where ~v:`Top))

  (** Dirty trick to only have vertical bars, and not the horizontal ones *)
  let matrix_with_column_spacers =
    let rec interleave sep = function
      | ([_] | []) as l -> l
      | hd :: tl -> hd :: sep :: interleave sep tl
    in
    List.map (fun l -> interleave (PrintBox.text " | ") l)

  let right_strip_columns (m : string list list as 'a) : 'a =
    let count_trailing_spaces s =
      let s =
        String.to_seq s |> List.of_seq |> List.rev |> List.to_seq
        |> String.of_seq
      in
      String.fold_left
        (fun acc c ->
          match (acc, c) with
          | (true, x), ' ' -> (true, x + 1)
          | (true, x), _ -> (false, x)
          | (false, _), _ -> acc)
        (true, 0)
        s
      |> snd
    in
    let m = transpose_matrix m in
    let m =
      List.map
        (fun col ->
          let min_space_suffix =
            List.fold_left
              (fun acc cell ->
                if cell = "" then acc else min acc (count_trailing_spaces cell))
              max_int
              col
          in
          List.map
            (function
              | "" -> ""
              | cell -> String.sub cell 0 (String.length cell - min_space_suffix))
            col)
        m
    in
    let m = transpose_matrix m in
    m

  let insert_dim_headers ~colnames ~rownames m =
    let ndim_col = List.length (List.hd colnames) in
    let ndim_row = List.length (List.hd rownames) in
    let m =
      List.init ndim_col (fun i ->
          List.map
            (fun l -> List.nth l i |> text |> align ~h:`Center ~v:`Top)
            colnames)
      @ m
    in
    let m =
      List.mapi
        (fun rowi row ->
          let rowi = rowi - ndim_col in
          let l =
            if rowi < 0 then List.init ndim_row (Fun.const (text ""))
            else
              List.nth rownames rowi |> List.map text
              |> List.map (align ~h:`Left ~v:`Top)
          in
          l @ row)
        m
    in
    m

  let rec text_of_t t =
    match view t with
    | Text {l = [txt]; _} -> txt
    | Empty -> assert false
    | Text _ -> assert false
    | Frame _ -> assert false
    | Pad _ -> assert false
    | Align {inner; _} -> text_of_t inner
    | Grid _ -> assert false
    | Tree _ -> assert false
    | Link _ -> assert false

  let insert_row_spacers ~should_put_space_before_row (m : t list list) =
    List.map
      (fun row ->
        let row' : string list = row |> List.map text_of_t in

        if should_put_space_before_row row' then
          [List.map (Fun.const (text "")) row; row]
        else [row])
      m
    |> List.flatten

  let blank_consecutive_equals_on_first_row (m : t list list) =
    let rec aux prev_value_opt = function
      | [] -> []
      | hd :: tl ->
          let hd_txt = text_of_t hd in
          let prev_value, hd =
            match prev_value_opt with
            | None -> (hd_txt, hd)
            | Some prev_txt when prev_txt = hd_txt -> (prev_txt, text "")
            | Some _ -> (hd_txt, hd)
          in
          hd :: aux (Some prev_value) tl
    in
    match m with [] -> [] | hd :: tl -> aux None hd :: tl
end

let reduce f l =
  match l with [] -> assert false | hd :: tl -> List.fold_left f hd tl

let add_rusage (x : Def.Gc.rusage) (y : Def.Gc.rusage) =
  let ( + ) = Int64.add in
  Def.Gc.
    {
      maxrss = x.maxrss + y.maxrss;
      minflt = x.minflt + y.minflt;
      majflt = x.majflt + y.majflt;
      inblock = x.inblock + y.inblock;
      oublock = x.oublock + y.oublock;
      nvcsw = x.nvcsw + y.nvcsw;
      nivcsw = x.nivcsw + y.nivcsw;
    }

let sum_rusage = reduce add_rusage

let add_duration (x : Def.Gc.duration) (y : Def.Gc.duration) =
  Def.Gc.
    {wall = x.wall +. y.wall; sys = x.sys +. y.sys; user = x.user +. y.user}

let sum_duration = reduce add_duration

let add_index (x : Def.Gc.index) (y : Def.Gc.index) =
  Def.Gc.
    {
      bytes_read = x.bytes_read + y.bytes_read;
      nb_reads = x.nb_reads + y.nb_reads;
      bytes_written = x.bytes_written + y.bytes_written;
      nb_writes = x.nb_writes + y.nb_writes;
    }

let sum_index = reduce add_index

let add_pack_store (x : Def.Gc.pack_store) (y : Def.Gc.pack_store) =
  Def.Gc.
    {
      appended_hashes = x.appended_hashes + y.appended_hashes;
      appended_offsets = x.appended_offsets + y.appended_offsets;
      total = x.total + y.total;
      from_staging = x.from_staging + y.from_staging;
      from_lru = x.from_lru + y.from_lru;
      from_pack_direct = x.from_pack_direct + y.from_pack_direct;
      from_pack_indexed = x.from_pack_indexed + y.from_pack_indexed;
    }

let sum_pack_store = reduce add_pack_store

let add_inode (x : Def.Gc.inode) (y : Def.Gc.inode) =
  Def.Gc.
    {
      inode_add = x.inode_add + y.inode_add;
      inode_remove = x.inode_remove + y.inode_remove;
      inode_of_seq = x.inode_of_seq + y.inode_of_seq;
      inode_of_raw = x.inode_of_raw + y.inode_of_raw;
      inode_rec_add = x.inode_rec_add + y.inode_rec_add;
      inode_rec_remove = x.inode_rec_remove + y.inode_rec_remove;
      inode_to_binv = x.inode_to_binv + y.inode_to_binv;
      inode_decode_bin = x.inode_decode_bin + y.inode_decode_bin;
      inode_encode_bin = x.inode_encode_bin + y.inode_encode_bin;
    }

let sum_inode = reduce add_inode

let add_ocaml_gc (x : Def.ocaml_gc) (y : Def.ocaml_gc) =
  Def.
    {
      minor_words = x.minor_words +. y.minor_words;
      promoted_words = x.promoted_words +. y.promoted_words;
      major_words = x.major_words +. y.major_words;
      minor_collections = x.minor_collections + y.minor_collections;
      major_collections = x.major_collections + y.major_collections;
      heap_words = x.heap_words + y.heap_words;
      compactions = x.compactions + y.compactions;
      top_heap_words = x.top_heap_words + y.top_heap_words;
      stack_size = x.stack_size + y.stack_size;
    }

let sum_ocaml_gc = reduce add_ocaml_gc

(*

| -- Config / Setup --
| < Same as in normal pp >

-- Main thread during GC --
|              summary name |      GC-run-1 |     GC-run-2 |
|    number of GCs averaged |             1 |            1 |
|                           | ------------- |------------- |
|                      %cpu |          100% |          99% |
|         total GC duration |
|   total finalise duration |
|       max commit duration |
|      mean commit duration |
|        mean find duration |
|         max find duration |
|                           |
|                    Blocks |        10_000 | 10_500 (+5%) |
|           TZ-Transactions |
|             TZ-Operations |
|                disk reads |
|           disk bytes read |
|               disk writes |
|        disk bytes written |
|                    minflt |
|                    majflt |
|                   inblock |
|                   oublock |
|                     nvcsw |
|                    nivcsw |
|                           |
|                Blocks/sec |
|       TZ-Transactions/sec |
|         TZ-Operations/sec |
|            disk reads/sec |
|       disk bytes read/sec |
|           disk writes/sec |
|    disk bytes written/sec |
|                minflt/sec |
|                majflt/sec |
|               inblock/sec |
|               oublock/sec |
|                 nvcsw/sec |
|                nivcsw/sec |
|                           |
|              minflt/block |
|              majflt/block |
|             inblock/block |
|             oublock/block |
|               nvcsw/block |
|              nivcsw/block |

| -- File sizes --
|                             summary name |      GC-run-1 |     GC-run-2 |
|                   number of GCs averaged |             1 |            1 |
|                                          | ------------- |------------- |
|             new suffix file size (bytes) |           5GB |   6GB (+20%) |
|             new prefix file size (bytes) |
|          new reachable file size (bytes) |
|             new sorted file size (bytes) |
|            new mapping file size (bytes) |
|                                          |
|          former suffix file size (bytes) |
|          former prefix file size (bytes) |
|         former mapping file size (bytes) |
|                                          |
|     suffix start offset progress (bytes) |
|               total file sizes before GC |
|                 total file before unlink |
|                  total file after unlink |
|                                          |
|       suffix newies in first worker loop |
|      suffix newies in extra worker loops |
|                suffix newies in finalise |

-- Worker stats --
|                             summary name |      GC-run-1 |     GC-run-2 |
|                   number of GCs averaged |             1 |            1 |
|                                          | ------------- |------------- |
|                    total duration (wall) |
|                    total duration (user) |
|                     total duration (sys) |
|                                     %cpu |
|                                     %sys |
|                                    %user |
|                        objects traversed |
|                    suffix transfer loops |
|                                          |
|                   initial maxrss (bytes) |
|                     final maxrss (bytes) |
|                                   minflt |
|                                   majflt |
|                                  inblock |
|                                  oublock |
|                                    nvcsw |
|                                   nivcsw |
|                                          |
|                               disk reads |
|                          disk bytes read |
|                              disk writes |
|                       disk bytes written |
|                                          |
|                          appended_hashes |
|                         appended_offsets |
|                                    total |
|                             from_staging |
|                                 from_lru |
|                         from_pack_direct |
|                        from_pack_indexed |
|                                          |
|                                inode_add |
|                             inode_remove |
|                             inode_of_seq |
|                             inode_of_raw |
|                            inode_rec_add |
|                         inode_rec_remove |
|                            inode_to_binv |
|                         inode_decode_bin |
|                         inode_encode_bin |
|                                          |
|                        initial heap byte |
|                   initial top heap bytes |
|                     final top heap bytes |
|                      initial stack bytes |
|                        final stack bytes |
|                              minor_words |
|                           promoted_words |
|                              major_words |
|                        minor_collections |
|                        major_collections |
|                              compactions |

-- Main timings per step --
|                   | elapsed wall | elapsed user | elapsed sys |
|   step0 |  run 0  |
|   step0 |  run 1  |
|   step1 |  run 0  |
|   step1 |  run 1  |

-- Worker timings per step --
< similar as main timings >

-- Worker rusage stats per step --
|                   | minflt | majflt | inblock | oublock | nvcsw | nivcsw |
|   step0 |  run 0  |
|   step0 |  run 1  |
|   step1 |  run 0  |
|   step1 |  run 1  |

-- Worker disk stats per step --
< similar as worker rusage stats >

-- Worker pack_store stats per step --
< similar as worker rusage stats >

-- Worker inode stats per step --
< similar as worker rusage stats >

-- Worker ocaml_gc stats per step --
< similar as worker rusage stats >

# durations: 6
# rusage: 7
# disk: 4
# pack_store: 7
# inode: 9
# ocaml_gc: 8

TODO: To the toposort for step names
TODO: It must be possible to input a process that had 0 GCs...
TODO: In the 3d plots (x:step*json, y:metric) use different color for each json. Need flag --no-color

Trucs a rajouter dans les stats d'irmin:

Il faut rajouter "old prefix", "old mapping" et "old suffix" and `worker.files`.

Il y a la fonction `Gc_stats.Worker.add_file_size` pour rajouter, mais c'est peut etre pas le plus simple. A voir.

Pour prefix et mapping tu peux mettre Int63.zero si y'en a pas.

Pour le suffix faut faire gaffe a pas compter les newies, du coup je sais pas trop ce qui est le plus simple.

*)

module Point = struct
  (** Multi dimensional key *)
  module Key = struct
    type t = string array [@@deriving repr ~pp]
  end

  module Float = struct
    type t = Key.t * float [@@deriving repr ~pp]

    module Frame = struct
      type point_float = t [@@deriving repr]

      type t = point_float list [@@deriving repr ~pp]

      (** Turn a float-frame into a string-frame using [formatter_of_group]
          in which we've grouped the float-points given [group_of_key]. *)
      let stringify ff ~group_of_key ~formatter_of_group =
        let examples = Hashtbl.create 0 in
        List.iter
          (fun (k, v) ->
            let g = group_of_key k in
            match Hashtbl.find_opt examples g with
            | None -> Hashtbl.add examples g [v]
            | Some l -> Hashtbl.replace examples g (v :: l))
          ff ;
        let formatters = Hashtbl.create 0 in
        Hashtbl.iter
          (fun g examples ->
            Hashtbl.add formatters g (formatter_of_group g examples))
          examples ;
        List.map
          (fun (k, v) ->
            let formatter = Hashtbl.find formatters (group_of_key k) in
            let v = Fmt.str "%a" formatter v in
            (k, v))
          ff

      let stringify_percents ff ~keep_blank ~group_of_key =
        let blank = "     " in
        let first_occ_per_group = Hashtbl.create 0 in
        List.map
          (fun (k, v) ->
            if keep_blank k then (k, blank)
            else
              let g = group_of_key k in
              match Hashtbl.find_opt first_occ_per_group g with
              | None ->
                  Hashtbl.add first_occ_per_group g v ;
                  (k, blank)
              | Some denom ->
                  let v = Fmt.str " %a" Utils.pp_percent (v /. denom) in
                  (k, v))
          ff
    end
  end

  module String = struct
    type t = Key.t * string [@@deriving repr ~pp]

    module Frame = struct
      type point_string = t [@@deriving repr]

      type t = point_string list [@@deriving repr ~pp]

      let concat : t -> t -> t =
       fun sf sf' ->
        assert (List.length sf = List.length sf') ;
        List.map2
          (fun (k, v) (k', v') ->
            assert (k = k') ;
            (k, v ^ v'))
          sf
          sf'

      let rec cartesian_product : 'a list list -> 'a list list = function
        | [] -> []
        | [l] -> List.map (fun x -> [x]) l
        | l :: ll ->
            let below = cartesian_product ll in
            List.map (fun ki -> List.map (fun l -> ki :: l) below) l
            |> List.flatten

      let filter_on_axis (sf : t) ~axis ~value : t =
        List.filter (fun (k, _v) -> k.(axis) = value) sf

      (** This is very similar to [pandas.DataFrame.pivot] *)
      let to_printbox (sf : t) ~col_axes ~row_axes ~should_put_space_before_row
          =
        assert (List.length col_axes >= 1) ;
        assert (List.length row_axes >= 1) ;
        assert (List.length sf >= 1) ;
        let ndim = Array.length (List.hd sf |> fst) in
        let values_per_dim = Array.init ndim (fun _ -> []) in
        List.iter
          (fun (k, _) ->
            Array.iteri
              (fun i ki ->
                if List.mem ki values_per_dim.(i) then ()
                else values_per_dim.(i) <- values_per_dim.(i) @ [ki])
              k)
          sf ;
        let rows =
          cartesian_product (List.map (Array.get values_per_dim) row_axes)
        in
        let cols =
          cartesian_product (List.map (Array.get values_per_dim) col_axes)
        in
        let matrix : string list list =
          List.filter_map
            (fun (row_values : string list) ->
              assert (List.length row_values = List.length row_axes) ;
              (* Filter [sf] to only keep the points of the current row *)
              let sf =
                List.fold_left2
                  (fun sf axis value -> filter_on_axis sf ~axis ~value)
                  sf
                  row_axes
                  row_values
              in
              let any_hit = ref false in
              let cells : string list =
                List.map
                  (fun (col_values : string list) ->
                    (* Filter [sf] to only keep the points of the current col *)
                    let sf =
                      List.fold_left2
                        (fun sf axis value -> filter_on_axis sf ~axis ~value)
                        sf
                        col_axes
                        col_values
                    in
                    match sf with
                    | [] -> ""
                    | [(_key, value)] ->
                        any_hit := true ;
                        value
                    | _ ->
                        (* several identical keys in [sf] *)
                        assert false)
                  cols
              in
              if !any_hit then Some cells else None)
            rows
        in
        matrix |> Pb.right_strip_columns |> Pb.matrix_to_text
        |> Pb.align_matrix `Right
        |> Pb.insert_dim_headers ~colnames:cols ~rownames:rows
        |> Pb.blank_consecutive_equals_on_first_row |> Pb.transpose_matrix
        |> Pb.blank_consecutive_equals_on_first_row |> Pb.transpose_matrix
        |> Pb.insert_row_spacers ~should_put_space_before_row
        |> Pb.matrix_with_column_spacers |> Pb.grid_l ~bars:false
    end
  end
end

module Table1 = struct
  let truc (summary_names, summaries) : Point.Float.Frame.t =
    let is_step_finalise = function
      | "worker startup" -> false
      | "before finalise" -> false
      | "worker wait" -> true
      | "read output" -> true
      | "copy latest newies" -> true
      | "swap and purge" -> true
      | "unlink" -> true
      | x ->
          Fmt.epr "Please edit code to classify: %S\n%!" x ;
          assert false
    in
    let ( let+ ) () f =
      List.map2 (fun x y -> f (x, y)) summary_names summaries |> List.flatten
    in
    let+ sname, s = () in

    let ( let+ ) () f =
      (* TODO: Average GCs *)
      f (List.hd s.gcs)
    in
    let+ (gc : Def.Gc.t) = () in
    let total_duration =
      List.map (fun (_, step) -> step.Def.Gc.duration) gc.worker.steps
      |> sum_duration
    in
    let finalise_duration =
      gc.steps
      |> List.filter (fun (stepname, _) -> is_step_finalise stepname)
      |> List.map snd |> sum_duration
    in
    let ff_of_timings stepname (d : Def.Gc.duration) =
      [
        ([|sname; stepname; "wall"|], d.wall);
        ([|sname; stepname; "share"|], d.wall /. total_duration.wall);
        ([|sname; stepname; "sys"|], d.sys);
        ([|sname; stepname; "user"|], d.user);
        ([|sname; stepname; "%cpu"|], (d.sys +. d.user) /. d.wall);
        ([|sname; stepname; "%sys"|], d.sys /. d.wall);
        ([|sname; stepname; "%user"|], d.user /. d.wall);
      ]
    in
    let ff_steps =
      let ( let+ ) () f = List.map f gc.steps |> List.flatten in
      let+ stepname, d = () in
      ff_of_timings stepname d
    in
    ff_of_timings "total" total_duration
    @ ff_of_timings "finalise" finalise_duration
    @ ff_steps

  let truc ppf (summary_names, summaries) =
    let ff = truc (summary_names, summaries) in
    let x =
      let group_of_key k = [k.(1); k.(2)] in
      let formatter_of_group g occurences =
        if List.exists (fun s -> String.contains s '%') g || List.mem "share" g
        then Utils.pp_percent
        else Utils.create_pp_seconds occurences
      in
      Point.Float.Frame.stringify ff ~group_of_key ~formatter_of_group
    in
    let y =
      let keep_blank =
        Array.exists (fun s -> String.contains s '%' || s = "share")
      in
      let group_of_key k = [k.(1); k.(2)] in
      Point.Float.Frame.stringify_percents ff ~keep_blank ~group_of_key
    in

    let sf = Point.String.Frame.concat x y in
    let s =
      let should_put_space_before_row = function
        | "worker startup" :: sname :: __ when sname = List.hd summary_names ->
            true
        | _ -> false
      in
      Point.String.Frame.to_printbox
        sf
        ~col_axes:[2]
        ~row_axes:[1; 0]
        ~should_put_space_before_row
      |> PrintBox_text.to_string
    in
    Fmt.pf ppf "%s" s
end

module Table2 = struct
  let truc (summary_names, summaries) : Point.Float.Frame.t =
    let ( let+ ) () f =
      List.map2 (fun x y -> f (x, y)) summary_names summaries |> List.flatten
    in
    let+ sname, s = () in

    let ( let+ ) () f =
      (* TODO: Average GCs *)
      f (List.hd s.gcs)
    in
    let+ (gc : Def.Gc.t) = () in

    let total_duration =
      List.map (fun (_, step) -> step.Def.Gc.duration) gc.worker.steps
      |> sum_duration
    in
    let ff_of_timings stepname (d : Def.Gc.duration) =
      [
        ([|sname; stepname; "wall"|], d.wall);
        ([|sname; stepname; "share"|], d.wall /. total_duration.wall);
        ([|sname; stepname; "sys"|], d.sys);
        ([|sname; stepname; "user"|], d.user);
        ([|sname; stepname; "%cpu"|], (d.sys +. d.user) /. d.wall);
        ([|sname; stepname; "%sys"|], d.sys /. d.wall);
        ([|sname; stepname; "%user"|], d.user /. d.wall);
      ]
    in
    let ff_steps =
      let ( let+ ) () f = List.map f gc.worker.steps |> List.flatten in
      let+ stepname, (d : Def.Gc.step) = () in
      ff_of_timings stepname d.duration
    in
    ff_of_timings "total" total_duration @ ff_steps

  let truc ppf (summary_names, summaries) =
    let ff = truc (summary_names, summaries) in
    let x =
      let group_of_key k = [k.(1); k.(2)] in
      let formatter_of_group g occurences =
        if List.exists (fun s -> String.contains s '%') g || List.mem "share" g
        then Utils.pp_percent
        else Utils.create_pp_seconds occurences
      in
      Point.Float.Frame.stringify ff ~group_of_key ~formatter_of_group
    in
    let y =
      let keep_blank =
        Array.exists (fun s -> String.contains s '%' || s = "share")
      in
      let group_of_key k = [k.(1); k.(2)] in
      Point.Float.Frame.stringify_percents ff ~keep_blank ~group_of_key
    in

    let sf = Point.String.Frame.concat x y in
    let s =
      let should_put_space_before_row = function
        | "open files" :: sname :: __ when sname = List.hd summary_names -> true
        | _ -> false
      in
      Point.String.Frame.to_printbox
        sf
        ~col_axes:[2]
        ~row_axes:[1; 0]
        ~should_put_space_before_row
      |> PrintBox_text.to_string
    in
    Fmt.pf ppf "%s" s
end

module Table3 = struct
  let truc (summary_names, summaries) : Point.Float.Frame.t =
    let ( let+ ) () f =
      List.map2 (fun x y -> f (x, y)) summary_names summaries |> List.flatten
    in
    let+ sname, s = () in

    let ( let+ ) () f =
      (* TODO: Average GCs *)
      f (List.hd s.gcs)
    in
    let+ (gc : Def.Gc.t) = () in
    let w : Def.Gc.worker = gc.worker in
    let _, (last_worker_step : Def.Gc.step) = List.rev w.steps |> List.hd in
    let rusage =
      List.map (fun (_, (step : Def.Gc.step)) -> step.rusage) w.steps
      |> sum_rusage
    in
    let total_duration =
      List.map (fun (_, step) -> step.Def.Gc.duration) gc.worker.steps
      |> sum_duration
    in
    let index =
      List.map (fun (_, (step : Def.Gc.step)) -> step.index) w.steps
      |> sum_index
    in
    let pack_store =
      List.map (fun (_, (step : Def.Gc.step)) -> step.pack_store) w.steps
      |> sum_pack_store
    in
    let inode =
      List.map (fun (_, (step : Def.Gc.step)) -> step.inode) w.steps
      |> sum_inode
    in
    let ocaml_gc =
      List.map (fun (_, (step : Def.Gc.step)) -> step.ocaml_gc) w.steps
      |> sum_ocaml_gc
    in
    let i = float_of_int in
    [
      ([|sname; "total duration (wall)"|], total_duration.wall);
      ([|sname; "total duration (sys)"|], total_duration.sys);
      ([|sname; "total duration (user)"|], total_duration.user);
      ( [|sname; "%cpu"|],
        (total_duration.sys +. total_duration.user) /. total_duration.wall );
      ([|sname; "%sys"|], total_duration.sys /. total_duration.wall);
      ([|sname; "%user"|], total_duration.user /. total_duration.wall);
      ([|sname; "objects traversed"|], w.objects_traversed |> Int63.to_float);
      ([|sname; "suffix transert loops"|], List.length w.suffix_transfers |> i);
      (* rusage *)
      ( [|sname; "initial maxrss (bytes)"|],
        w.initial_maxrss |> Int64.to_float |> ( *. ) 1000. );
      ( [|sname; "final maxrss (bytes)"|],
        last_worker_step.rusage.maxrss |> Int64.to_float |> ( *. ) 1000. );
      ([|sname; "minflt"|], rusage.minflt |> Int64.to_float);
      ([|sname; "majflt"|], rusage.majflt |> Int64.to_float);
      ([|sname; "inblock"|], rusage.inblock |> Int64.to_float);
      ([|sname; "oublock"|], rusage.oublock |> Int64.to_float);
      ([|sname; "nvcsw"|], rusage.nvcsw |> Int64.to_float);
      ([|sname; "nivcsw"|], rusage.nivcsw |> Int64.to_float);
      (* disk *)
      ([|sname; "disk reads"|], index.nb_reads |> i);
      ([|sname; "disk bytes read"|], index.bytes_read |> i);
      ([|sname; "disk writes"|], index.nb_writes |> i);
      ([|sname; "disk bytes written"|], index.bytes_written |> i);
      (* pack_store *)
      ([|sname; "appended_hashes"|], pack_store.appended_hashes |> i);
      ([|sname; "appended_offsets"|], pack_store.appended_offsets |> i);
      ([|sname; "total"|], pack_store.total |> i);
      ([|sname; "from_staging"|], pack_store.from_staging |> i);
      ([|sname; "from_lru"|], pack_store.from_lru |> i);
      ([|sname; "from_pack_direct"|], pack_store.from_pack_direct |> i);
      ([|sname; "from_pack_indexed"|], pack_store.from_pack_indexed |> i);
      (* inode *)
      ([|sname; "inode_add"|], inode.inode_add |> i);
      ([|sname; "inode_remove"|], inode.inode_remove |> i);
      ([|sname; "inode_of_seq"|], inode.inode_of_seq |> i);
      ([|sname; "inode_of_raw"|], inode.inode_of_raw |> i);
      ([|sname; "inode_rec_add"|], inode.inode_rec_add |> i);
      ([|sname; "inode_rec_remove"|], inode.inode_rec_remove |> i);
      ([|sname; "inode_to_binv"|], inode.inode_to_binv |> i);
      ([|sname; "inode_decode_bin"|], inode.inode_decode_bin |> i);
      ([|sname; "inode_encode_bin"|], inode.inode_encode_bin |> i);
      (* ocaml_gc *)
      ([|sname; "initial heap byte"|], w.initial_heap_words * 8 |> i);
      ([|sname; "initial top heap bytes"|], w.initial_top_heap_words * 8 |> i);
      ( [|sname; "  final top heap bytes"|],
        last_worker_step.ocaml_gc.top_heap_words * 8 |> i );
      ([|sname; "initial stack bytes"|], w.initial_stack_size * 8 |> i);
      ( [|sname; "  final stack bytes"|],
        last_worker_step.ocaml_gc.stack_size * 8 |> i );
      ([|sname; "minor_words"|], ocaml_gc.minor_words);
      ([|sname; "promoted_words"|], ocaml_gc.promoted_words);
      ([|sname; "major_words"|], ocaml_gc.major_words);
      ([|sname; "minor_collections"|], ocaml_gc.minor_collections |> i);
      ([|sname; "major_collections"|], ocaml_gc.major_collections |> i);
      ([|sname; "compactions"|], ocaml_gc.compactions |> i);
    ]

  let truc ppf (summary_names, summaries) =
    let ff = truc (summary_names, summaries) in
    let x =
      let group_of_key k =
        match k.(1) with
        | "initial maxrss (bytes)" | "final maxrss (bytes)" -> ["maxrss"]
        | _ -> [k.(1)]
      in
      let formatter_of_group g occurences =
        if List.exists (fun s -> String.contains s '%') g || List.mem "share" g
        then Utils.pp_percent
        else if List.exists (String.starts_with ~prefix:"total duration") g then
          Utils.create_pp_seconds occurences
        else Utils.create_pp_real occurences
      in
      Point.Float.Frame.stringify ff ~group_of_key ~formatter_of_group
    in
    let y =
      let keep_blank = Array.exists (fun s -> String.contains s '%') in
      let group_of_key k = [k.(1)] in
      Point.Float.Frame.stringify_percents ff ~keep_blank ~group_of_key
    in

    let sf = Point.String.Frame.concat x y in
    let s =
      let should_put_space_before_row = function
        | ( "initial maxrss (bytes)" | "initial heap byte" | "inode_add"
          | "appended_hashes" | "disk reads" )
          :: _ ->
            true
        | _ -> false
      in
      Point.String.Frame.to_printbox
        sf
        ~col_axes:[0]
        ~row_axes:[1]
        ~should_put_space_before_row
      |> PrintBox_text.to_string
    in
    Fmt.pf ppf "%s" s
end

let pp_gcs ppf (summary_names, summaries) =
  Format.fprintf
    ppf
    {|
-- Worker stats --
%a

-- Main timings --
%a

-- Worker timings --
%a



|}
    Table3.truc
    (summary_names, summaries)
    Table1.truc
    (summary_names, summaries)
    Table2.truc
    (summary_names, summaries)
