(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Brassaia-tezos
    Invocation:   dune exec brassaia/test/brassaia-tezos/main.exe
    Subject: This file is the entrypoint of Brassaia-tezos Tezt tests. It
    dispatches to other files.
*)

let () = Generate.register ()
