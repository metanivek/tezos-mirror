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
    List.map (fun l ->
        PrintBox.text " | " :: interleave (PrintBox.text " | ") l)

  (* let interleave m m' =
   *   List.map2 (fun row row' ->
   *       List.map2 (fun cell cell' -> [cell; cell']) row row' |> List.flatten
   *     ) m m' *)

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
      List.map (fun col ->
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
            col) m
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
end

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
|                                    %user |
|                                     %sys |
|                                     %cpu |
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


TODO:
- blank redundant header cells
- implement %cpu in main timings table


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

      (** Turn a float-frame into a string-frame using [Utils.create_pp_seconds]
          in which we've grouped the float-points given [group_of_key]. *)
      let stringify_seconds ff group_of_key =
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
            Hashtbl.add formatters g (Utils.create_pp_seconds examples))
          examples ;
        List.map
          (fun (k, v) ->
            let formatter = Hashtbl.find formatters (group_of_key k) in
            let v = Fmt.str "%a" formatter v in
            (k, v))
          ff

      let stringify_percents ff group_of_key =
        let first_occ_per_group = Hashtbl.create 0 in
        List.map
          (fun (k, v) ->
            let g = group_of_key k in
            match Hashtbl.find_opt first_occ_per_group g with
            | None ->
                Hashtbl.add first_occ_per_group g v ;
                (k, "     ")
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
      let to_printbox (sf : t) ~col_axes ~row_axes =
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
                        value (* Pb.text value |> Pb.align ~h:`Right ~v:`Top *)
                    | _ ->
                        (* several identical keys in [sf] *)
                        assert false)
                  cols
              in
              if !any_hit then Some cells else None)
            rows
        in
        matrix |> Pb.right_strip_columns
        |> Pb.matrix_to_text
        |> Pb.align_matrix `Right
        |> Pb.insert_dim_headers ~colnames:cols ~rownames:rows
        |> Pb.matrix_with_column_spacers |> Pb.grid_l ~bars:false
    end
  end
end

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

  let ( let+ ) () f = List.map f gc.steps |> List.flatten in
  let+ stepname, (d : Def.Gc.duration) = () in

  [
    ([|sname; stepname; "wall"|], d.wall);
    ([|sname; stepname; "sys"|], d.sys);
    ([|sname; stepname; "user"|], d.user);
    (* ([|sname; stepname; "%cpu"|], (d.sys +. d.user) /. d.wall); *)
  ]

let pp_gcs ppf (summary_names, summaries) =
  let ff = truc (summary_names, summaries) in
  Fmt.pf ppf "%a\n%!" Point.Float.Frame.pp ff ;

  let x = Point.Float.Frame.stringify_seconds ff (fun k -> [k.(1); k.(2)]) in
  let y = Point.Float.Frame.stringify_percents ff (fun k -> [k.(1); k.(2)]) in
  let sf = Point.String.Frame.concat x y in

  (* Fmt.pf ppf "%a\n%!" Point.String.Frame.pp sf ; *)
  let s =
    Point.String.Frame.to_printbox sf ~col_axes:[2] ~row_axes:[1; 0]
    |> PrintBox_text.to_string
  in
  Fmt.pf ppf "%s\n" s ;

  let s =
    Point.String.Frame.to_printbox sf ~col_axes:[2; 0] ~row_axes:[1]
    |> PrintBox_text.to_string
  in
  Fmt.pf ppf "%s" s ;

  ()
(* ignore (summary_names, summaries) *)
(* List.iter2 (fun (sname, s) ->) summary_names, summaries; *)
(* Format.fprintf ppf "DELICIEUX" *)
