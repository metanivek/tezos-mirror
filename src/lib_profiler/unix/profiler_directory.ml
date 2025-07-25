(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

let build_rpc_directory () =
  let module Registered = (val Profiler_services.registered_module ()) in
  let dir = Tezos_rpc.Directory.empty in
  let dir =
    Tezos_rpc.Directory.register0 dir Registered.S.registered (fun () () ->
        let registered_backends =
          match Profiler_instance.selected_backends () with
          | Some backends ->
              List.map
                (fun backend_info -> backend_info.Profiler_instance.view)
                backends
          | None -> []
        in
        let backends =
          Profiler_instance.BackendMap.bindings
            !Profiler_instance.registered_backends
          |> List.map (fun (env_var, infos) ->
                 (env_var, infos.Profiler_instance.view))
        in
        Lwt.return_ok Registered.{registered_backends; backends})
  in
  dir
