(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(* This module defines the jobs of the [release_tag] family of pipelines.

   These pipeline runs on each pushes to the various release tags (see
   [main.ml] for the set of regular expressions that define the
   language of release tags).

   The goal of these pipelines is to create
   {{:https://gitlab.com/tezos/tezos/-/releases}Octez releases on
   GitLab}, the associated artifacts, and to push releases to opam. *)

open Tezos_ci
open Common

(** Type of release tag pipelines.

    The semantics of the type is summed up in this table:

   |                       | Release_tag | Beta_release_tag | Non_release_tag |
   |-----------------------+-------------+------------------+-----------------|
   | GitLab release type   | Release     | Release          | Create          |
   | Experimental binaries | No          | No               | No              |
   | Docker build type     | Release     | Release          | Release         |
   | Publishes to opam     | Yes         | No               | No              |

    - All release tag pipelines types publish [Release] type Docker builds.
    - No release tag pipelines include experimental binaries.
    - [Release_tag] and [Beta_release_tag] pipelines creates GitLab
    and publishes releases. [Non_release_tag] pipelines create the
    GitLab release but do not publish them.
    - Only [Release_tag] pipelines publish to opam. *)
type release_tag_pipeline_type =
  | Release_tag
  | Beta_release_tag
  | Non_release_tag
  | Schedule_test

let monitoring_child_pipeline =
  Pipeline.register_child
    "octez_monitoring"
    ~description:"Octez monitoring jobs"
    ~inherit_:
      (Gitlab_ci.Types.Variable_list
         ["ci_image_name"; "ci_image_name_protected"; "jsonnet_image_name"])
    ~jobs:
      [
        job_datadog_pipeline_trace;
        job_build_layer1_profiling ~expire_in:Never ();
        Teztale.Common.job_build ~expire_in:Never ~arch:Arm64 ~storage:Ramfs ();
        Teztale.Common.job_build ~expire_in:Never ~arch:Amd64 ~cpu:Very_high ();
      ]

let job_release_page ~test ?dependencies () =
  job
    ~__POS__
    ~image:Images.ci_release
    ~stage:Stages.publish
    ~description:
      "A job to update the Octez release page. If running in a test pipleine, \
       the assets are pushed in the [release-page-test.nomadic-labs.com] \
       bucket. Otherwise they are pushed in [site.prod.octez.tezos.com]. Then \
       its [index.html] is updated accordingly."
    ~name:"publish:release-page"
    ~rules:[Gitlab_ci.Util.job_rule ~when_:Manual ()]
    ~artifacts:
      (Gitlab_ci.Util.artifacts
         ~expire_in:(Duration (Days 1))
         ["./index.md"; "index.html"])
    ?dependencies
    ~variables:
      (if test then
         (* The S3_BUCKET, AWS keys and DISTRIBUTION_ID
            depends on the release type (tests or not). *)
         [
           ("S3_BUCKET", "release-page-test.nomadic-labs.com");
           ("DISTRIBUTION_ID", "E19JF46UG3Z747");
           ("AWS_ACCESS_KEY_ID", "${AWS_KEY_RELEASE_PUBLISH}");
           ("AWS_SECRET_ACCESS_KEY", "${AWS_SECRET_RELEASE_PUBLISH}");
         ]
       else
         [
           ("S3_BUCKET", "site-prod.octez.tezos.com");
           ("BUCKET_PATH", "/releases");
           ("URL", "octez.tezos.com");
           ("DISTRIBUTION_ID", "${CLOUDFRONT_DISTRIBUTION_ID}");
         ])
    ["./scripts/releases/publish_release_page.sh"]
    ~retry:Gitlab_ci.Types.{max = 0; when_ = []}

(* Temporary job that uses [bin_release_page/release_page.ml]
   to update the release page.
   This will be removed once [job_release_page] uses it. *)
let job_update_release_page ~test () =
  job
    ~__POS__
    ~image:Images.CI.build
    ~stage:Stages.publish
    ~description:""
    ~name:"publish:update-release-page"
    ~rules:[Gitlab_ci.Util.job_rule ~when_:Manual ()]
    ~artifacts:
      (Gitlab_ci.Util.artifacts
         ~expire_in:(Duration (Days 1))
         ["index.md"; "index.html"])
    ~variables:
      (if test then
         (* The S3_BUCKET, AWS keys and DISTRIBUTION_ID
            depends on the release type (tests or not). *)
         [
           ("S3_BUCKET", "release-page-test.nomadic-labs.com");
           ("DISTRIBUTION_ID", "E19JF46UG3Z747");
           ("AWS_ACCESS_KEY_ID", "${AWS_KEY_RELEASE_PUBLISH}");
           ("AWS_SECRET_ACCESS_KEY", "${AWS_SECRET_RELEASE_PUBLISH}");
         ]
       else
         [
           ("S3_BUCKET", "site-prod.octez.tezos.com");
           ("BUCKET_PATH", "/releases");
           ("URL", "octez.tezos.com");
           ("DISTRIBUTION_ID", "${CLOUDFRONT_DISTRIBUTION_ID}");
         ])
    ~before_script:["eval $(opam env)"]
    ~after_script:["cp /tmp/release_page*/index.md ./index.md"]
    [
      "sudo apk add aws-cli pandoc";
      "dune exec ./ci/bin_release_page/release_page.exe -- --component 'octez' \
       --title 'Octez releases' --bucket ${S3_BUCKET} --path \
       '${BUCKET_PATH:-}' changelog binaries packages";
      "aws s3 cp \"./index.html\" \"s3://${S3_BUCKET}${BUCKET_PATH}/\"";
      "aws cloudfront create-invalidation --distribution-id \
       \"$DISTRIBUTION_ID\" --paths \"/*\"";
    ]

(** Create an Octez release tag pipeline of type {!release_tag_pipeline_type}.

    If [test] is true (default is [false]), then the Docker images are
    built of the [Test] type and are published to the GitLab registry
    instead of Docker hub.

    If [major] is false (default is [true]), then components jobs are
    excluded from the Octez jobs.

    On release pipelines these jobs can start immediately *)
let octez_jobs ?(test = false) ?(major = true) release_tag_pipeline_type =
  let variables =
    match release_tag_pipeline_type with
    | Schedule_test -> Some [("CI_COMMIT_TAG", "octez-v0.0")]
    | _ -> None
  in
  let job_docker_amd64 =
    job_docker_build
      ~dependencies:(Dependent [])
      ~__POS__
      ~arch:Amd64
      (if test then Test else Release)
  in
  let job_docker_arm64 =
    job_docker_build
      ~dependencies:(Dependent [])
      ~__POS__
      ~arch:Arm64
      ~storage:Ramfs
      (if test then Test else Release)
  in
  let job_docker_merge =
    job_docker_merge_manifests
      ~__POS__
      ~ci_docker_hub:(not test)
      ~job_docker_amd64
      ~job_docker_arm64
  in
  (* on release pipelines the static binaries do not have any dependencies
     on previous stages and can start immediately *)
  let job_static_arm64_release =
    job_build_static_binaries
      ~dependencies:(Dependent [])
      ~__POS__
      ~arch:Arm64
      ~storage:Ramfs
      ~release:true
      ()
  in
  let job_static_x86_64_release =
    job_build_static_binaries
      ~dependencies:(Dependent [])
      ~__POS__
      ~arch:Amd64
      ~cpu:Very_high
      ~storage:Ramfs
      ~release:true
      ()
  in
  let job_build_homebrew_release =
    let artifacts =
      Gitlab_ci.Util.artifacts
        ~expire_in:(Duration (Days 1))
        ~name:"build-$CI_COMMIT_REF_SLUG"
        ~when_:On_success
        ["public/homebrew/*"]
    in
    job
      ~__POS__
      ~name:"oc.install-release-homebrew"
      ~arch:Amd64
      ~dependencies:(Dependent [])
      ~image:Images.debian_bookworm
      ~stage:Stages.build
      ~artifacts
      [
        "./scripts/ci/install-gsutil.sh";
        "apt install -y git build-essential";
        "./scripts/packaging/homebrew_install.sh";
        "eval $(/home/linuxbrew/.linuxbrew/bin/brew shellenv)";
        "./scripts/packaging/homebrew_release.sh";
      ]
  in
  let job_gitlab_release ~dependencies : Tezos_ci.tezos_job =
    job
      ~__POS__
      ~image:Images.ci_release
      ~stage:Stages.publish
      ~interruptible:false
      ~dependencies:(Dependent (Job job_docker_merge :: dependencies))
      ~id_tokens:Tezos_ci.id_tokens
      ~name:"gitlab:release"
      ?variables
      [
        "./scripts/ci/restrict_export_to_octez_source.sh";
        "./scripts/ci/gitlab-release.sh";
      ]
      ~retry:Gitlab_ci.Types.{max = 0; when_ = []}
  in
  let job_gitlab_publish ~dependencies () : Tezos_ci.tezos_job =
    let before_script =
      match release_tag_pipeline_type with
      | Schedule_test -> Some ["git tag octez-v0.0"]
      | _ -> None
    in
    job
      ~__POS__
      ~image:Images.ci_release
      ~stage:Stages.publish
      ~interruptible:false
      ~dependencies:(Dependent dependencies)
      ?before_script
      ?variables
      ~id_tokens:Tezos_ci.id_tokens
      ~name:"gitlab:publish"
      [
        ("${CI_PROJECT_DIR}/scripts/ci/create_gitlab_package.sh"
        ^
        match release_tag_pipeline_type with
        | Schedule_test -> " --dry-run"
        | _ -> "");
      ]
      ~retry:Gitlab_ci.Types.{max = 0; when_ = []}
  in
  let jobs_dnf_repository = Rpm_repository.jobs Release in
  let jobs_debian_repository = Debian_repository.jobs Release in
  let job_gitlab_release_or_publish =
    let dependencies =
      [
        Artifacts job_static_x86_64_release;
        Artifacts job_static_arm64_release;
        Artifacts job_build_homebrew_release;
      ]
    in
    match release_tag_pipeline_type with
    | Non_release_tag | Schedule_test -> job_gitlab_publish ~dependencies ()
    | _ -> job_gitlab_release ~dependencies
  in
  let job_release_page =
    job_release_page
      ~test
      ~dependencies:
        (Dependent
           [
             Artifacts job_static_x86_64_release;
             Artifacts job_static_arm64_release;
           ])
      ()
  in
  let job_opam_release ?(dry_run = false) () : Tezos_ci.tezos_job =
    job
      ~__POS__
      ~image:Images.CI.prebuild
      ~stage:Stages.publish
      ~description:
        "Update opam package descriptions on tezos/tezos opam-repository fork.\n\n\
         This job does preliminary work for releasing Octez opam packages on \
         opam repository, by pushing a branch with updated package \
         descriptions (.opam files) to \
         https://github.com/tezos/opam-repository. It _does not_ automatically \
         create a corresponding pull request on the official opam repository."
      ~interruptible:false
      ?variables
      ~name:"opam:release"
      [("./scripts/ci/opam-release.sh" ^ if dry_run then " --dry-run" else "")]
      ~retry:Gitlab_ci.Types.{max = 0; when_ = []}
  in
  let job_promote_to_latest_test =
    Common.job_docker_promote_to_latest
      ~ci_docker_hub:false
      ~dependencies:(Dependent [Job job_docker_merge])
      ()
  in
  let job_trigger_monitoring =
    trigger_job
      ~__POS__
      ~dependencies:(Dependent [])
      ~stage:Stages.build
      monitoring_child_pipeline
  in
  [
    (* Stage: start *)
    job_datadog_pipeline_trace;
    (* Stage: build *)
    job_static_x86_64_release;
    job_static_arm64_release;
    job_docker_amd64;
    job_docker_arm64;
    job_build_homebrew_release;
    job_docker_merge;
    job_gitlab_release_or_publish;
    job_trigger_monitoring;
  ]
  @ jobs_debian_repository @ jobs_dnf_repository
  (* Include components release jobs only if this is a major release. *)
  @ (if not major then []
     else
       match (test, release_tag_pipeline_type) with
       | false, (Release_tag | Beta_release_tag | Non_release_tag) ->
           !Tezos_ci.Hooks.global_release
           @ Teztale.Release.jobs ~test:false ~dry_run:false ()
       | true, (Release_tag | Beta_release_tag | Non_release_tag) ->
           !Tezos_ci.Hooks.global_test_release
           @ Teztale.Release.jobs ~test:true ~dry_run:false ()
       | true, Schedule_test ->
           !Tezos_ci.Hooks.global_scheduled_test_release
           @ Teztale.Release.jobs ~test:true ~dry_run:true ()
       | false, Schedule_test ->
           failwith
             "test = false is inconsistent with release_tag_pipeline_type = \
              Schedule_test")
  @
  match (test, release_tag_pipeline_type) with
  (* for the moment the apt repository are not official, so we do not add to the release
     pipeline . *)
  | false, Release_tag -> [job_opam_release (); job_release_page]
  | true, Release_tag ->
      [
        (* This job normally runs in the {!Octez_latest_release} pipeline
           that is triggered manually after a release is made. However, to
           make release testing easier, we include it here directly. Thus,
           release testers are not required to trigger two separate pipelines
           (indeed, the second `latest_release_test` pipeline is rarely tested). *)
        job_promote_to_latest_test;
        job_opam_release ~dry_run:true ();
        job_release_page;
      ]
  | _ -> []

(** Create an etherlink release tag pipeline of type {!release_tag_pipeline_type}. *)
let octez_evm_node_jobs ?(test = false) () =
  let job_docker_amd64 =
    job_docker_build
      ~__POS__
      ~arch:Amd64
      (if test then Test else Octez_evm_node_release)
  in
  let job_docker_arm64 =
    job_docker_build
      ~__POS__
      ~arch:Arm64
      ~storage:Ramfs
      (if test then Test else Octez_evm_node_release)
  in
  let job_docker_merge =
    job_docker_merge_manifests
      ~__POS__
      ~ci_docker_hub:(not test)
      ~job_docker_amd64
      ~job_docker_arm64
  in
  let job_static_x86_64_release =
    job_build_static_binaries
      ~__POS__
      ~arch:Amd64
      ~cpu:Very_high
      ~storage:Ramfs
      ~executable_files:"script-inputs/octez-evm-node-executable"
      ~release:true
      ~version_executable:"octez-evm-node"
      ()
  in
  let job_static_arm64_release =
    job_build_static_binaries
      ~__POS__
      ~arch:Arm64
      ~storage:Ramfs
      ~executable_files:"script-inputs/octez-evm-node-executable"
      ~release:true
      ~version_executable:"octez-evm-node"
      ()
  in

  let job_gitlab_release : Tezos_ci.tezos_job =
    let dependencies =
      Dependent
        [
          Artifacts job_static_x86_64_release; Artifacts job_static_arm64_release;
        ]
    in
    job
      ~__POS__
      ~image:Images.ci_release
      ~stage:Stages.publish
      ~interruptible:false
      ~dependencies
      ~name:"gitlab:octez-evm-node-release"
      ~description:"Create a GitLab release for Etherlink"
      ["./scripts/ci/create_gitlab_octez_evm_node_release.sh"]
      ~retry:Gitlab_ci.Types.{max = 0; when_ = []}
  in
  let job_docker_promote_to_latest ~ci_docker_hub () : tezos_job =
    job_docker_authenticated
      ~__POS__
      ~dependencies:(Dependent [Job job_docker_merge])
      ~stage:Stages.publish
      ~name:"docker:promote_to_latest"
      ~ci_docker_hub
      [
        "./scripts/ci/docker_promote_to_latest.sh octez-evm-node-latest \
         ./scripts/ci/octez-evm-node-release.sh";
      ]
      ~retry:Gitlab_ci.Types.{max = 0; when_ = []}
  in
  [
    (* Stage: start *)
    job_datadog_pipeline_trace;
    (* Stage: build *)
    job_static_arm64_release;
    job_static_x86_64_release;
    job_docker_amd64;
    job_docker_arm64;
    job_docker_merge;
    job_gitlab_release;
    job_docker_promote_to_latest ~ci_docker_hub:(not test) ();
  ]
