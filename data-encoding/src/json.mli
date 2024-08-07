(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** Like most other [.mli] files in this directory, this is not intended for
    end-users. Instead, the interface from this file is used internally to
    assemble the end-user-intended module {!Data_encoding}. Refer to that module
    for documentation. *)

type json =
  [ `O of (string * json) list
  | `Bool of bool
  | `Float of float
  | `A of json list
  | `Null
  | `String of string ]

type t = json

type schema = Json_schema.schema

val convert : 'a Encoding.t -> 'a Json_encoding.encoding

val schema : ?definitions_path:string -> 'a Encoding.t -> schema

val encoding : json Encoding.t

val schema_encoding : schema Encoding.t

val construct :
  ?include_default_fields:[`Always | `Auto | `Never] ->
  't Encoding.t ->
  't ->
  json

val destruct : ?bson_relaxation:bool -> 't Encoding.t -> json -> 't

type path = path_item list

and path_item = [`Field of string | `Index of int | `Star | `Next]

exception Cannot_destruct of (path * exn)

exception Unexpected of string * string

exception No_case_matched of exn list

exception Bad_array_size of int * int

exception Missing_field of string

exception Unexpected_field of string

val print_error :
  ?print_unknown:(Format.formatter -> exn -> unit) ->
  Format.formatter ->
  exn ->
  unit

val cannot_destruct : ('a, Format.formatter, unit, 'b) format4 -> 'a

val wrap_error : ('a -> 'b) -> 'a -> 'b

val from_string : string -> (json, string) result

val to_string : ?newline:bool -> ?minify:bool -> json -> string

val pp : Format.formatter -> json -> unit

val bytes : Encoding.string_json_repr -> bytes Json_encoding.encoding

val string : Encoding.string_json_repr -> string Json_encoding.encoding

type jsonm_lexeme =
  [ `Null
  | `Bool of bool
  | `String of string
  | `Float of float
  | `Name of string
  | `As
  | `Ae
  | `Os
  | `Oe ]

val construct_seq : 't Encoding.t -> 't -> jsonm_lexeme Seq.t
