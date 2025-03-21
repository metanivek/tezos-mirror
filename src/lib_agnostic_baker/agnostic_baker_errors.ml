(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

type error +=
  | Lost_node_connection
  | Cannot_connect_to_node of string
  | Cannot_decode_node_data of string
  | Missing_current_baker
  | Baker_process_error

let () =
  Error_monad.register_error_kind
    `Permanent
    ~id:"agnostic_baker.lost_node_connection"
    ~title:"Lost node connection"
    ~description:"Connection with node lost."
    ~pp:(fun ppf () -> Format.fprintf ppf "Connection with node was lost")
    Data_encoding.(unit)
    (function Lost_node_connection -> Some () | _ -> None)
    (fun () -> Lost_node_connection) ;
  Error_monad.register_error_kind
    `Permanent
    ~id:"agnostic_baker.cannot_connect_to_node"
    ~title:"Cannot connect to node"
    ~description:"Cannot connect to node."
    ~pp:(fun ppf uri ->
      Format.fprintf
        ppf
        "Cannot connect to node. Connection refused (ECONNREFUSED): %s"
        uri)
    Data_encoding.(obj1 (req "uri" string))
    (function Cannot_connect_to_node uri -> Some uri | _ -> None)
    (fun uri -> Cannot_connect_to_node uri) ;
  Error_monad.register_error_kind
    `Permanent
    ~id:"agnostic_baker.cannot_decode_node_data"
    ~title:"Cannot decode node data"
    ~description:"Cannot decode node data."
    ~pp:(fun ppf err -> Format.fprintf ppf "Cannot decode node data: %s" err)
    Data_encoding.(obj1 (req "err" string))
    (function Cannot_decode_node_data err -> Some err | _ -> None)
    (fun err -> Cannot_decode_node_data err) ;
  Error_monad.register_error_kind
    `Permanent
    ~id:"agnostic_baker.missing_current_baker"
    ~title:"Missing current baker"
    ~description:"The current baker process is missing."
    ~pp:(fun ppf () -> Format.fprintf ppf "Missing current baker")
    Data_encoding.(unit)
    (function Missing_current_baker -> Some () | _ -> None)
    (fun () -> Missing_current_baker) ;
  Error_monad.register_error_kind
    `Permanent
    ~id:"agnostic_baker.baker_process_error"
    ~title:"Underlying baker process error"
    ~description:"There is an error in the underlying baker process."
    ~pp:(fun ppf () ->
      Format.fprintf ppf "Error in the underlying baker process")
    Data_encoding.(unit)
    (function Baker_process_error -> Some () | _ -> None)
    (fun () -> Baker_process_error)
