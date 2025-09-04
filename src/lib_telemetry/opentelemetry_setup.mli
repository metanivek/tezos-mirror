(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

(** Set Opentelemetry global options and registers the Lwt compatible ambient
    context. [?level] and [?sections] are used to specify which events are
    exported by Opentelemetry as logs. *)
val setup :
  data_dir:string ->
  service_namespace:string ->
  service_name:string ->
  version:string ->
  ?level:Internal_event.level ->
  ?sections:(Internal_event.Section.t * Internal_event.level) list ->
  Opentelemetry_config.t ->
  unit tzresult Lwt.t
