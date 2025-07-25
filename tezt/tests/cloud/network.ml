(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

type public =
  [ `Mainnet
  | `Ghostnet
  | `Nextnet of string
  | `Weeklynet of string
  | `Rionet
  | `Seoulnet ]

type t = [`Sandbox | public]

let public_encoding =
  let open Data_encoding in
  union
    [
      case
        (Tag 2)
        ~title:"mainnet"
        empty
        (function `Mainnet -> Some () | _ -> None)
        (fun () -> `Mainnet);
      case
        (Tag 3)
        ~title:"ghostnet"
        empty
        (function `Ghostnet -> Some () | _ -> None)
        (fun () -> `Ghostnet);
      case
        (Tag 4)
        ~title:"nextnet"
        string
        (function `Nextnet date -> Some date | _ -> None)
        (fun date -> `Nextnet date);
      case
        (Tag 5)
        ~title:"weeklynet"
        string
        (function `Weeklynet date -> Some date | _ -> None)
        (fun date -> `Weeklynet date);
      case
        (Tag 6)
        ~title:"rionet"
        empty
        (function `Rionet -> Some () | _ -> None)
        (fun () -> `Rionet);
      case
        (Tag 7)
        ~title:"seoulnet"
        empty
        (function `Seoulnet -> Some () | _ -> None)
        (fun () -> `Seoulnet);
    ]

let encoding =
  let open Data_encoding in
  union
    [
      case
        (Tag 1)
        ~title:"sandbox"
        empty
        (function `Sandbox -> Some () | _ -> None)
        (fun () -> `Sandbox);
      case
        (Tag 2)
        ~title:"mainnet"
        empty
        (function `Mainnet -> Some () | _ -> None)
        (fun () -> `Mainnet);
      case
        (Tag 3)
        ~title:"ghostnet"
        empty
        (function `Ghostnet -> Some () | _ -> None)
        (fun () -> `Ghostnet);
      case
        (Tag 4)
        ~title:"nextnet"
        string
        (function `Nextnet date -> Some date | _ -> None)
        (fun date -> `Nextnet date);
      case
        (Tag 5)
        ~title:"weeklynet"
        string
        (function `Weeklynet date -> Some date | _ -> None)
        (fun date -> `Weeklynet date);
      case
        (Tag 6)
        ~title:"rionet"
        empty
        (function `Rionet -> Some () | _ -> None)
        (fun () -> `Rionet);
      case
        (Tag 7)
        ~title:"seoulnet"
        empty
        (function `Seoulnet -> Some () | _ -> None)
        (fun () -> `Seoulnet);
    ]

type stake_repartition =
  | Custom of int list
  | Mimic of {network : public; max_nb_bakers : int option}

let stake_repartition_encoding =
  let open Data_encoding in
  union
    [
      case
        (Tag 1)
        ~title:"custom"
        (list int31)
        (function Custom l -> Some l | _ -> None)
        (fun l -> Custom l);
      case
        (Tag 2)
        ~title:"mimic"
        (obj2 (req "network" public_encoding) (opt "max_nb_bakers" int31))
        (function
          | Mimic {network; max_nb_bakers} -> Some (network, max_nb_bakers)
          | _ -> None)
        (fun (network, max_nb_bakers) -> Mimic {network; max_nb_bakers});
    ]

let to_public = function
  | `Sandbox -> failwith "Sandbox is not public"
  | #public as p -> p

let to_string = function
  | `Mainnet -> "mainnet"
  | `Ghostnet -> "ghostnet"
  | `Nextnet _ -> "nextnet"
  | `Weeklynet date -> sf "weeklynet-%s" date
  | `Sandbox -> "sandbox"
  | `Rionet -> "rionet"
  | `Seoulnet -> "seoulnet"

let parse = function
  | "mainnet" -> Some `Mainnet
  | "ghostnet" -> Some `Ghostnet
  | "rionet" -> Some `Rionet
  | "seoulnet" -> Some `Seoulnet
  | s when String.length s = 20 && String.sub s 0 10 = "weeklynet-" ->
      (* format: weeklynet-2025-01-29 (with dashes) *)
      let date = String.sub s 10 10 in
      Some (`Weeklynet date)
  | s when String.length s = 16 && String.sub s 0 8 = "nextnet-" ->
      (* format: nextnet-20250203 (without dashes) *)
      let date = String.sub s 8 8 in
      Some (`Nextnet date)
  | "sandbox" -> Some `Sandbox
  | _ -> None

let default_protocol : t -> Protocol.t = function
  | `Mainnet -> R022
  | `Ghostnet -> R022
  | `Weeklynet _ -> Alpha
  | `Sandbox -> Alpha
  | `Nextnet _ -> S023
  | `Rionet -> R022
  | `Seoulnet -> S023

let block_time : t -> int = function
  | `Mainnet -> 8
  | `Ghostnet -> 5
  | _ -> failwith "Block times are only available for Mainnet and Ghostnet."

let public_rpc_endpoint testnet =
  Endpoint.make
    ~scheme:"https"
    ~host:
      (match testnet with
      | `Mainnet -> "rpc.tzbeta.net"
      | `Ghostnet -> "rpc.ghostnet.teztnets.com"
      | `Nextnet date -> sf "rpc.nextnet-%s.teztnets.com" date
      | `Weeklynet date -> sf "rpc.weeklynet-%s.teztnets.com" date
      | `Rionet -> "rpc.rionet.teztnets.com"
      | `Seoulnet -> "rpc.seoulnet.teztnets.com")
    ~port:443
    ()

let snapshot_service = function
  | `Mainnet -> "https://snapshots.eu.tzinit.org/mainnet"
  | `Ghostnet -> "https://snapshots.eu.tzinit.org/ghostnet"
  | `Nextnet _ -> "https://snapshots.eu.tzinit.org/nextnet"
  | `Weeklynet _ -> "https://snapshots.eu.tzinit.org/weeklynet"
  | `Rionet -> "https://snapshots.eu.tzinit.org/rionet"
  | `Seoulnet -> "https://snapshots.eu.tzinit.org/seoulnet"

(* Argument to give to the --network option of `octez-node config init`. *)
let to_octez_network_options = function
  | `Mainnet -> "mainnet"
  | `Ghostnet -> "ghostnet"
  | `Nextnet date -> sf "https://teztnets.com/nextnet-%s" date
  | `Weeklynet date -> sf "https://teztnets.com/weeklynet-%s" date
  | `Rionet -> "https://teztnets.com/rionet"
  | `Seoulnet -> "https://teztnets.com/seoulnet"

let default_bootstrap = function
  | `Mainnet -> "boot.tzinit.org"
  | `Ghostnet -> "ghostnet.tzinit.org" (* Taken from ghostnet configuration *)
  | `Nextnet date -> sf "nextnet-%s.teztnets.com" date
  | `Weeklynet date -> sf "weeklynet-%s.teztnets.com" date
  | `Rionet -> "rionet.teztnets.com"
  | `Seoulnet -> "seoulnet.teztnets.com"

let default_dal_bootstrap = function
  | `Mainnet -> "dalboot.mainnet.tzboot.net"
  | `Ghostnet ->
      "dalboot.ghostnet.tzboot.net" (* Taken from ghostnet configuration *)
  | `Nextnet date -> sf "dal.nextnet-%s.teztnets.com" date
  | `Weeklynet date -> sf "dal.weeklynet-%s.teztnets.com" date
  | `Rionet -> "dal.rionet.teztnets.com"
  | `Seoulnet -> "dal.seoulnet.teztnets.com"

let get_level endpoint =
  let* json = RPC_core.call endpoint (RPC.get_chain_block_header_shell ()) in
  JSON.(json |-> "level" |> as_int) |> Lwt.return

let expected_pow = function `Sandbox -> 0. | _ -> 26.

let versions network =
  match network with
  | (`Mainnet | `Nextnet _ | `Ghostnet) as public_network -> (
      let decoder json =
        json |> JSON.as_list |> List.to_seq
        |> Seq.map (fun json_account ->
               JSON.
                 ( json_account |-> "address" |> as_string,
                   json_account |-> "software" |-> "version" |> as_string_opt ))
        |> Seq.filter_map (function
               | address, None -> Some (address, "unknown")
               | address, Some version -> Some (address, version))
        |> Hashtbl.of_seq
      in
      let rpc =
        RPC_core.(
          make
            ~query_string:[("limit", "5000"); ("active", "true")]
            GET
            ["v1"; "delegates"]
            decoder)
      in
      let endpoint =
        Endpoint.make
          ~host:(Format.asprintf "api.%s.tzkt.io" (to_string public_network))
          ~scheme:"https"
          ~port:443
          ()
      in
      let* response = RPC_core.call_raw endpoint rpc in
      try
        RPC_core.decode_raw ~origin:"Network.versions" rpc response.body
        |> Lwt.return_some
      with exn ->
        Log.warn
          "Unexpected error while fetching versions (code: %d): '%s'"
          response.code
          (Printexc.to_string exn) ;
        Lwt.return_none)
  | `Weeklynet _ | `Rionet | `Seoulnet ->
      (* No easy way to get this information. *)
      Lwt.return_some (Hashtbl.create 0)
  | `Sandbox ->
      (* Not sure what to do here since it depends on the docker image. We can
         figure that out later. *)
      Lwt.return_some (Hashtbl.create 0)

let delegates ?(accounts = []) network =
  match network with
  | (`Mainnet | `Ghostnet | `Rionet | `Seoulnet) as network -> (
      let decoder json =
        json |> JSON.as_list
        |> List.map (fun json_account ->
               JSON.
                 ( json_account |-> "alias" |> as_string_opt,
                   json_account |-> "address" |> as_string,
                   json_account |-> "publicKey" |> as_string ))
      in
      let rpc =
        RPC_core.(
          make
            ~query_string:[("limit", "5000"); ("active", "true")]
            GET
            ["v1"; "delegates"]
            decoder)
      in
      let endpoint =
        Endpoint.make
          ~host:(Format.asprintf "api.%s.tzkt.io" (to_string network))
          ~scheme:"https"
          ~port:443
          ()
      in
      let* response = RPC_core.call_raw endpoint rpc in
      try
        RPC_core.decode_raw ~origin:"Network.aliases" rpc response.body
        |> Lwt.return_some
      with exn ->
        Log.warn
          "Unexpected error while fetching aliases (code: %d): '%s'"
          response.code
          (Printexc.to_string exn) ;
        Lwt.return_none)
  | `Weeklynet _ | `Nextnet _ ->
      (* There are no aliases for these networks. *)
      Lwt.return_none
  | `Sandbox ->
      accounts
      |> List.map (fun account ->
             ( Some account.Account.alias,
               account.public_key_hash,
               account.public_key ))
      |> Lwt.return_some

let aliases ?accounts network =
  let* aliases = delegates ?accounts network in
  match aliases with
  | None -> Lwt.return_none
  | Some aliases ->
      aliases |> List.to_seq
      |> Seq.filter_map (function
             | None, _, _ -> None
             | Some alias, address, _ -> Some (address, alias))
      |> Hashtbl.of_seq |> Lwt.return_some
