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

-- Bench config/setup --
< Same as in normal pp >

-- File sizes --
                            summary name |      GC-run-1 |     GC-run-2 |
                  number of GCs averaged |             1 |            1 |
                                         | ------------- |------------- |
                suffix file size (bytes) |           5GB |   6GB (+20%) |
                prefix file size (bytes) |
             reachable file size (bytes) |
                sorted file size (bytes) |
               mapping file size (bytes) |
TODO: What about peak disk usage? Maybe include file sizes of old generation. (e.g. cumulative file size before unlink).

-- Worker stats --
                            summary name |      GC-run-1 |     GC-run-2 |
                  number of GCs averaged |             1 |            1 |
                                         | ------------- |------------- |
               initial heap_byte (bytes) |
                  initial maxrss (bytes) |
                    final maxrss (bytes) |
          initial top_heap_bytes (bytes) |
            final top_heap_bytes (bytes) |
              initial stack_size (bytes) |
                final stack_size (bytes) |
                       objects traversed |
                   suffix transfer loops |
                                   %user |
                                    %sys |
                                    %cpu |
TODO: How much has the suffix_start_offset progressed (i.e. how much GC has removed)
TODO: What was the state of the store before GC (i.e. prefix or not?)
TODO: Final value for all counters of rusage/ocaml_gc/index/pack_store/inode

-- Suffix bytes --
             summary name |      GC-run-1 |
   number of GCs averaged |               |
                          | bytes | share |
                          | ------------- |
                    Total | 10GB  | 100%  |
          Worker 1st loop | 5GB   | 50%   |
       Worker extra loops |
                 Finalise |
TODO: Make it an information of how much the suffix grew during the GC
- after_end_offset - before_end_offset (i.e. how much main has progressed)

-- Worker timings --
            |                   GC-run-1           |
  span-name | d-wall | share-wall | d-user | d-sys |
"to reacha" |
"to sorted" |

-- Worker stats --
      metric name |   inode_add   | inode_remove  |
                  | run-1 | run-2 | run-1 | run-2 |
  step0           |
  step1           |

-- Worker stats alternative --
      metric name |   inode_add   | inode_remove  |
  step0 |  run 0  |
  step0 |  run 1  |
  step1 |  run 0  |
  step1 |  run 1  |

-- Main thread during GC --
             summary name |      GC-run-1 |     GC-run-2 |
   number of GCs averaged |             1 |            1 |
                          | ------------- |------------- |
                     %cpu |          100% |          99% |
                   Blocks |        10_000 | 10_500 (+5%) |
          TZ-Transactions |
            TZ-Operations |
               disk reads |
          disk bytes read |
              disk writes |
       disk bytes written |
               Blocks/sec |
      TZ-Transactions/sec |
        TZ-Operations/sec |
           disk reads/sec |
      disk bytes read/sec |
          disk writes/sec |
   disk bytes written/sec |
TODO: tail latency
TODO: rusage




Plan if one GC:
- Amount of newies bytes dealt by the finalise
- Amount of newies bytes dealt by the worker (sum of iterations [1:])
- Size of all files (suffix requires a hack)
- The 4 initial ints
- Objects traversed
- %CPU
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
TODO: rusage for main process

Trucs a rajouter dans les stats d'irmin:
- bool: store before has prefix or not


*)

let pp_gcs ppf (summary_names, summaries) =
  ignore (summary_names, summaries);
  Format.fprintf ppf "DELICIEUX"
