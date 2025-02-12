(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Brassaia
    Invocation:   dune exec brassaia/test/main.exe
    Subject:      This file is the entrypoint of all Brassaia Tezt tests. It dispatches to
            other files.
*)

let () = Test_lib_brassaia_store.register ()
