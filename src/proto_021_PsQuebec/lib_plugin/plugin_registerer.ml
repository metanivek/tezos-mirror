(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2021 Nomadic Development. <contact@tezcore.com>             *)
(* Copyright (c) 2024 TriliTech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

module Validation = struct
  include Registerer.Registered
  module Plugin = Plugin.Mempool

  (* /!\ This overwrite must not be removed when protocol Q gets
     frozen. See {!Ghostnet_fix}. *)
  let finalize_application application_state shell_header =
    let open Lwt_syntax in
    let* application_state =
      Ghostnet_fix.fix_ghostnet_state application_state
    in
    finalize_application application_state shell_header
end

module RPC = struct
  module Proto = Registerer.Registered
  include Plugin.RPC
end

module Metrics = struct
  include Plugin.Metrics

  let hash = Registerer.Registered.hash
end

module Http_cache_headers = struct
  include Plugin.Http_cache_headers

  let hash = Registerer.Registered.hash
end

module Shell_helpers = struct
  include Plugin.Shell_helpers

  let hash = Registerer.Registered.hash
end

let () = Protocol_plugin.register_validation_plugin (module Validation)

let () = Protocol_plugin.register_rpc (module RPC)

let () = Protocol_plugin.register_metrics (module Metrics)

let () =
  Protocol_plugin.register_http_cache_headers_plugin (module Http_cache_headers)

let () = Protocol_plugin.register_shell_helpers (module Shell_helpers)
