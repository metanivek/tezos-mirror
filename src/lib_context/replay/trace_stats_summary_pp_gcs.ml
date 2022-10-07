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
end

(*
Plan if one GC:
- Amount of newies dealt by the finalise
- Amount of newies dealt by the worker (sum of iterations [1:])
- Size of all files (suffix requires a hack)
- The 4 initial ints
- Objects traversed
- Suffix transfers
- t.steps:
  - All atom spans + total + finalise
  - span-name | d-wall | share-wall | d-user | share-user | d-sys | share-sys
- t.worker.steps durations:
  - All atom spans + total
  - span-name | d-wall | share-wall | d-user | share-user | d-sys | share-sys
- TODO: rusage
- TODO: ocaml_gc
- TODO: index
- TODO: pack_store
- TODO: inode
- TODO: %CPU

Infos about the main process
- I might need to record in the stats trace when the gc starts
- I might need to make a more intelligent to-summary
- %CPU
- Tail latency (percentile or max?)

Data about the main process:
- Number of blocks (+ per sec)
- TZ-transactions (+ per sec)
- TZ-operations (+ per sec)
- Disk reads  (+ per sec)
- Disk writes (+ per sec)

TODO: To the toposort for step names

*)

let pp_gcs ppf (summary_names, summaries) =
  ignore (summary_names, summaries);
  Format.fprintf ppf "DELICIEUX"
