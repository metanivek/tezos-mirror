(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

module Agent = Agent
module Types = Types

module Alert = struct
  include Alert_manager

  type t = Alert_manager.alert

  type severity = Prometheus.severity = Critical | Warning | Info

  let make ?route ?for_ ?description ?summary ?severity ?group_name ?interval
      ~name ~expr () =
    let alert =
      Prometheus.make_alert
        ?for_
        ?description
        ?summary
        ?severity
        ?group_name
        ?interval
        ~name
        ~expr
        ()
    in

    Alert_manager.alert ?route alert
end

module Cloud = Cloud

let register_docker_push ~tags =
  Cloud.register
    ?vms:None
    ~__FILE__
    ~title:"Push the dockerfile to the GCP registry"
    ~tags:("docker" :: "push" :: tags)
  @@ fun _cloud -> Jobs.docker_build ~push:true ()

let register_docker_build ~tags =
  Cloud.register
    ?vms:None
    ~__FILE__
    ~title:"Build the dockerfile"
    ~tags:("docker" :: "build" :: tags)
  @@ fun _cloud -> Jobs.docker_build ~push:false ()

let register_deploy_docker_registry ~tags =
  Cloud.register
    ?vms:None
    ~__FILE__
    ~title:"Deploy docker registry"
    ~tags:("docker" :: "registry" :: "deploy" :: tags)
  @@ fun _cloud -> Jobs.deploy_docker_registry ()

let register_destroy_vms ~tags =
  Cloud.register
    ?vms:None
    ~__FILE__
    ~title:"Destroy terraform VMs"
    ~tags:("terraform" :: "destroy" :: "vms" :: tags)
  @@ fun _cloud ->
  let tezt_cloud = Env.tezt_cloud in
  let* project_id = Gcloud.project_id () in
  let* workspaces = Terraform.VM.Workspace.list ~tezt_cloud in
  let* () = Terraform.VM.destroy workspaces ~project_id in
  Terraform.VM.Workspace.destroy ~tezt_cloud

let register_prometheus_import ~tags =
  Cloud.register
    ?vms:None
    ~__FILE__
    ~title:"Import a snapshot into a prometheus container"
    ~tags:("prometheus" :: "import" :: tags)
  @@ fun _cloud ->
  let* prometheus = Prometheus.run_with_snapshot () in
  Prometheus.shutdown prometheus

let register_clean_up_vms ~tags =
  Cloud.register
    ?vms:None
    ~__FILE__
    ~title:"Clean ups VMs manually"
    ~tags:("clean" :: "up" :: tags)
  @@ fun _cloud -> Jobs.clean_up_vms ()

let register_list_vms ~tags =
  Cloud.register
    ?vms:None
    ~__FILE__
    ~title:"List VMs"
    ~tags:("list" :: "vms" :: tags)
  @@ fun _cloud ->
  Log.info "TEZT_CLOUD environment variable found with value: %s" Env.tezt_cloud ;
  let* _ = Gcloud.list_vms ~prefix:Env.tezt_cloud in
  Lwt.return_unit

let register_create_dns_zone ~tags =
  Cloud.register
    ?vms:None
    ~__FILE__
    ~title:"Create a new DNS zone"
    ~tags:("create" :: "dns" :: "zone" :: tags)
  @@ fun _cloud ->
  let* domains = Env.dns_domains () in
  match domains with
  | [] ->
      Test.fail "You must specify the domains to use via --dns-domain option."
  | domains ->
      Lwt_list.iter_p
        (fun domain ->
          let* res = Gcloud.DNS.find_zone_for_subdomain domain in
          match res with
          | Some (zone, _) ->
              let* () = Gcloud.DNS.create_zone ~domain ~zone () in
              let* _ = Gcloud.DNS.describe ~zone () in
              unit
          | None -> unit)
        domains

let register_describe_dns_zone ~tags =
  Cloud.register
    ?vms:None
    ~__FILE__
    ~title:"Describe a new DNS zone"
    ~tags:("describe" :: "dns" :: "zone" :: tags)
  @@ fun _cloud ->
  let* zones = Gcloud.DNS.list_zones () in
  Lwt_list.iter_s
    (fun (zone, _) ->
      let* _ = Gcloud.DNS.describe ~zone () in
      unit)
    zones

let register_list_dns_domains ~tags =
  Cloud.register
    ?vms:None
    ~__FILE__
    ~title:"List the DNS domains currently in use"
    ~tags:("describe" :: "dns" :: "list" :: tags)
  @@ fun _cloud ->
  let* zones = Gcloud.DNS.list_zones () in
  Lwt_list.iter_s
    (fun (zone, _) ->
      let* _ = Gcloud.DNS.list ~zone () in
      unit)
    zones

let register_dns_add ~tags =
  Cloud.register
    ?vms:None
    ~__FILE__
    ~title:"Register a new DNS entry"
    ~tags:("dns" :: "add" :: tags)
  @@ fun _cloud ->
  let ip =
    match Cli.get_string_opt "ip" with
    | None -> Test.fail "You must provide an IP address via -a ip=<ip>"
    | Some ip -> ip
  in
  let domain =
    match Cli.get_string_opt "dns-domain" with
    | None ->
        Test.fail
          "You must provide a domain name via -a dns-domain=<domain>. The \
           format expected is the same one as the CLI argument '--dns-domain' \
           of tezt-cloud. "
    | Some domain -> domain
  in
  let* res = Gcloud.DNS.find_zone_for_subdomain domain in
  let zone =
    match res with
    | None -> Test.fail "No suitable zone for %s" domain
    | Some (zone, _) -> zone
  in
  Gcloud.DNS.add_subdomain ~zone ~name:domain ~value:ip

let register_dns_remove ~tags =
  Cloud.register
    ?vms:None
    ~__FILE__
    ~title:"Remove a DNS entry"
    ~tags:("dns" :: "remove" :: tags)
  @@ fun _cloud ->
  let* domains = Env.dns_domains () in
  Lwt_list.iter_s
    (fun domain ->
      let* res = Gcloud.DNS.find_zone_for_subdomain domain in
      let zone =
        match res with
        | None -> Test.fail "No suitable zone for %s" domain
        | Some (zone, _) -> zone
      in
      let* ip = Gcloud.DNS.get_value ~zone ~domain in
      match ip with
      | None -> Test.fail "No record found for the current domain"
      | Some ip ->
          let* () = Gcloud.DNS.remove_subdomain ~zone ~name:domain ~value:ip in
          unit)
    domains

let register ~tags =
  register_docker_push ~tags ;
  register_docker_build ~tags ;
  register_deploy_docker_registry ~tags ;
  register_destroy_vms ~tags ;
  register_prometheus_import ~tags ;
  register_clean_up_vms ~tags ;
  register_list_vms ~tags ;
  register_create_dns_zone ~tags ;
  register_describe_dns_zone ~tags ;
  register_list_dns_domains ~tags ;
  register_dns_add ~tags ;
  register_dns_remove ~tags
