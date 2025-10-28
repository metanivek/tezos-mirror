(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

module Files = struct
  let sdks = ["src/kernel_sdk/**/*"; "sdk/rust/**/*"]

  let rust_toolchain_image =
    [
      "images/rust-toolchain/**/*";
      "images/create_image.sh";
      "images/scripts/install_datadog_static.sh";
      "scripts/version.sh";
    ]

  let lib_wasm_runtime_rust = ["src/lib_wasm_runtime/**/*.rs"]

  let node = ["etherlink/**/*"]

  let kernel = ["etherlink.mk"; "etherlink/**/*.rs"]

  let firehose =
    [
      "etherlink/firehose/**/*";
      "etherlink/tezt/tests/evm_kernel_inputs/erc20tok.*";
    ]

  let evm_compatibility =
    [
      "etherlink.mk";
      "etherlink/kernel_latest/revm/**/*";
      "etherlink/kernel_latest/evm_evaluation/**/*";
    ]

  let revm_compatibility =
    [
      "etherlink.mk";
      "etherlink/kernel_latest/revm/**/*";
      "etherlink/kernel_latest/revm_evaluation/**/*";
    ]

  (* [firehose], [evm_compatibility] and [revm_compatibility] are already included
     in [node @ kernel] *)
  let all = sdks @ rust_toolchain_image @ lib_wasm_runtime_rust @ node @ kernel
end

module CI = Cacio.Make (struct
  let name = "etherlink"

  let paths = Files.all
end)

let job_build_evm_node_static =
  Cacio.parameterize @@ fun arch ->
  CI.job
    ("build_evm_node_static_"
    ^ Tezos_ci.Runner.Arch.show_easy_to_distinguish arch)
    ~__POS__
    ~stage:Test
    ~description:"Build the EVM node (statically linked)."
    ~arch
    ?cpu:(match arch with Amd64 -> Some Very_high | Arm64 -> None)
    ?storage:(match arch with Arm64 -> Some Ramfs | Amd64 -> None)
    ~image:Tezos_ci.Images.CI.build
    ~only_if_changed:Files.(node @ sdks)
    ~artifacts:
      (Gitlab_ci.Util.artifacts
         ~name:"evm-binaries"
         ~when_:On_success
         ["octez-evm-*"; "etherlink-*"])
    ~cargo_cache:true
    ~cargo_target_caches:true
    ~sccache:(Cacio.sccache ~cache_size:"2G" ())
    [
      "./scripts/ci/take_ownership.sh";
      ". ./scripts/version.sh";
      "eval $(opam env)";
      "make evm-node-static";
    ]

let job_lint_wasm_runtime =
  CI.job
    "lint_wasm_runtime"
    ~__POS__
    ~stage:Test
    ~description:"Run the linter on lib_wasm_runtime."
    ~image:Tezos_ci.Images.CI.build
    ~only_if_changed:Files.lib_wasm_runtime_rust
    ~cargo_cache:true
    ~sccache:(Cacio.sccache ())
    [
      "./scripts/ci/take_ownership.sh";
      ". ./scripts/version.sh";
      "eval $(opam env)";
      "etherlink/lib_wasm_runtime/lint.sh";
    ]

let job_unit_tests =
  CI.job
    "unit_tests"
    ~__POS__
    ~stage:Test
    ~description:"Etherlink unit tests."
    ~image:Tezos_ci.Images.CI.build
    ~only_if_changed:Files.(node @ sdks)
    ~artifacts:
      ((* Note: the [~name] is actually overridden by the one computed
           by [Tezos_ci.Coverage.enable_output_artifact].
           We set it anyway for consistency with how the job
           was previously declared using [job_unit_test] in [code_verification.ml]. *)
       Gitlab_ci.Util.artifacts
         ~name:"$CI_JOB_NAME-$CI_COMMIT_SHA-x86_64"
         ["test_results"]
         ~reports:(Gitlab_ci.Util.reports ~junit:"test_results/*.xml" ())
         ~expire_in:(Duration (Days 1))
         ~when_:Always)
    ~cargo_cache:true
    ~sccache:(Cacio.sccache ())
    ~dune_cache:(Cacio.dune_cache ())
    ~test_coverage:true
    ~variables:[("DUNE_ARGS", "-j 12")]
    ~retry:{max = 2; when_ = []}
    [". ./scripts/version.sh"; "eval $(opam env)"; "make test-etherlink-unit"]

let job_test_kernel =
  Cacio.parameterize @@ fun pipeline_type ->
  CI.job
    "test_kernel"
    ~__POS__
    ~stage:Test
    ~description:"Check and test the etherlink kernel."
    ~image:Tezos_ci.Images.rust_toolchain
    ~only_if_changed:Files.(rust_toolchain_image @ kernel @ sdks)
    ~needs_legacy:
      [(Job, Tezos_ci_jobs.Code_verification.job_build_kernels pipeline_type)]
    ~variables:[("CC", "clang"); ("NATIVE_TARGET", "x86_64-unknown-linux-musl")]
    ~cargo_cache:true
    ~sccache:(Cacio.sccache ())
    ["make -f etherlink.mk check"; "make -f etherlink.mk test"]

let job_test_firehose =
  Cacio.parameterize @@ fun pipeline_type ->
  CI.job
    "test_firehose"
    ~__POS__
    ~stage:Test
    ~description:"Check and test etherlink firehose."
    ~image:Tezos_ci.Images.rust_toolchain
    ~only_if_changed:Files.(rust_toolchain_image @ firehose)
    ~needs_legacy:
      [(Job, Tezos_ci_jobs.Code_verification.job_build_kernels pipeline_type)]
    ~variables:[("CC", "clang"); ("NATIVE_TARGET", "x86_64-unknown-linux-musl")]
    ~cargo_cache:true
    ~sccache:(Cacio.sccache ())
    ["make -C etherlink/firehose check"]

let job_test_evm_compatibility =
  Cacio.parameterize @@ fun pipeline_type ->
  CI.job
    "test_evm_compatibility"
    ~__POS__
    ~stage:Test
    ~description:"Check and test EVM compatibility."
    ~image:Tezos_ci.Images.rust_toolchain
    ~only_if_changed:Files.(rust_toolchain_image @ evm_compatibility)
    ~needs_legacy:
      [(Job, Tezos_ci_jobs.Code_verification.job_build_kernels pipeline_type)]
    ~variables:[("CC", "clang"); ("NATIVE_TARGET", "x86_64-unknown-linux-musl")]
    ~cargo_cache:true
    ~sccache:(Cacio.sccache ())
    [
      "make -f etherlink.mk EVM_EVALUATION_FEATURES=disable-file-logs \
       evm-evaluation-assessor";
      "git clone --depth 1 --branch v14.1@etherlink \
       https://github.com/functori/tests ethereum_tests";
      "./evm-evaluation-assessor --eth-tests ./ethereum_tests/ --resources \
       ./etherlink/kernel_latest/evm_evaluation/resources/ -c";
    ]

let job_test_revm_compatibility =
  Cacio.parameterize @@ fun pipeline_type ->
  CI.job
    "test_revm_compatibility"
    ~__POS__
    ~stage:Test
    ~description:"Check and test REVM compatibility."
    ~image:Tezos_ci.Images.rust_toolchain
    ~only_if_changed:Files.(rust_toolchain_image @ revm_compatibility)
    ~needs_legacy:
      [(Job, Tezos_ci_jobs.Code_verification.job_build_kernels pipeline_type)]
    ~variables:[("CC", "clang"); ("NATIVE_TARGET", "x86_64-unknown-linux-musl")]
    ~cargo_cache:true
    ~sccache:(Cacio.sccache ())
    [
      "make -f etherlink.mk EVM_EVALUATION_FEATURES=disable-file-logs \
       revm-evaluation-assessor";
      "git clone --depth 1 https://github.com/functori/evm-fixtures \
       evm_fixtures";
      "./revm-evaluation-assessor --test-cases ./evm_fixtures/";
    ]

let job_build_tezt =
  CI.job
    "build_tezt"
    ~__POS__
    ~stage:Build
    ~description:"Build the Etherlink Tezt executable."
    ~image:Tezos_ci.Images.CI.build
    ~artifacts:
      (Gitlab_ci.Util.artifacts
         ~name:"etherlink_tezt_exe"
         ~when_:On_success
         ~expire_in:(Duration (Days 1))
         ["_build/default/etherlink/ci/tezt/main.exe"])
    ~dune_cache:
      (Cacio.dune_cache
         ~key:
           ("dune-build-cache-"
           ^ Gitlab_ci.Predefined_vars.(show ci_pipeline_id))
         ())
    ~cargo_cache:true
    ~sccache:(Cacio.sccache ())
    ~cargo_target_caches:true
    [
      "./scripts/ci/take_ownership.sh";
      ". ./scripts/version.sh";
      "eval $(opam env)";
      "dune build etherlink/ci/tezt/main.exe";
    ]

(* Specialization of Cacio's [tezt_job] with defaults that are specific to this component. *)
(* Note: for now the changeset is the same as the one for regular Tezt jobs,
   but to follow the vision for the monorepo, it should be limited to Etherlink.
   This is something that can be done later, once we feel ready. *)
let tezt_job ?(retry_tests = 1) =
  CI.tezt_job
    ~tezt_exe:"etherlink/ci/tezt/main.exe"
    ~fetch_records_from:"etherlink.daily"
    ~only_if_changed:
      (Tezos_ci.Changeset.encode Tezos_ci_jobs.Changesets.changeset_octez)
    ~needs:[(Artifacts, job_build_tezt)]
    ~needs_legacy:
      [
        ( Artifacts,
          Tezos_ci_jobs.Code_verification.job_build_x86_64_release
            Before_merging );
        ( Artifacts,
          Tezos_ci_jobs.Code_verification.job_build_x86_64_extra_exp
            Before_merging );
        ( Artifacts,
          Tezos_ci_jobs.Code_verification.job_build_x86_64_extra_dev
            Before_merging );
        ( Artifacts,
          Tezos_ci_jobs.Code_verification.job_build_kernels Before_merging );
      ]
    ~retry_tests

let job_tezt =
  Cacio.parameterize @@ fun pipeline ->
  tezt_job
    ""
    ~pipeline
    ~description:"Run normal Etherlink Tezt tests."
    ~test_coverage:true
    ~test_selection:
      (Tezos_ci_jobs.Tezt.tests_tag_selector [Not (Has_tag "flaky")])
    ~parallel_jobs:17
    ~parallel_tests:6
    ~retry_jobs:2

let job_tezt_slow =
  Cacio.parameterize @@ fun pipeline ->
  tezt_job
    "slow"
    ~pipeline
    ~description:"Run Etherlink Tezt tests tagged as slow."
    ~test_selection:(Tezos_ci_jobs.Tezt.tests_tag_selector ~slow:true [])
    ~test_timeout:No_timeout
    ~parallel_jobs:6
    ~parallel_tests:3
    ~retry_jobs:2

let job_tezt_extra =
  Cacio.parameterize @@ fun pipeline ->
  tezt_job
    "extra"
    ~pipeline
    ~description:"Run Etherlink Tezt tests tagged as extra and not flaky."
    ~test_selection:
      (Tezos_ci_jobs.Tezt.tests_tag_selector
         ~extra:true
         [Not (Has_tag "flaky")])
    ~parallel_jobs:5
    ~parallel_tests:6
    ~retry_jobs:2

let job_tezt_flaky =
  Cacio.parameterize @@ fun pipeline ->
  tezt_job
    "flaky"
    ~pipeline
    ~description:"Run Etherlink Tezt tests tagged as flaky."
    ~test_coverage:true
    ~allow_failure:Yes
    ~test_selection:(Tezos_ci_jobs.Tezt.tests_tag_selector [Has_tag "flaky"])
    ~retry_jobs:2
    ~retry_tests:3

let register () =
  CI.register_before_merging_jobs
    [
      (Manual, job_build_evm_node_static Amd64);
      (Manual, job_build_evm_node_static Arm64);
      (Auto, job_lint_wasm_runtime);
      (Auto, job_unit_tests);
      (* We rely on the fact that [Tezos_ci_pipelines.Code_verification.job_build_kernels]
         returns an equivalent job for [Before_merging] and [Merge_train]. *)
      (Auto, job_test_kernel Before_merging);
      (Auto, job_test_firehose Before_merging);
      (Auto, job_test_evm_compatibility Before_merging);
      (Auto, job_test_revm_compatibility Before_merging);
      (Auto, job_tezt `merge_request);
      (Manual, job_tezt_slow `merge_request);
      (Manual, job_tezt_extra `merge_request);
      (Manual, job_tezt_flaky `merge_request);
    ] ;
  CI.register_scheduled_pipeline
    "daily"
    ~description:"Daily tests to run for Etherlink."
    ~legacy_jobs:
      [
        Tezos_ci_jobs.Code_verification.job_build_x86_64_release
          Schedule_extended_test;
        Tezos_ci_jobs.Code_verification.job_build_x86_64_extra_exp
          Schedule_extended_test;
        Tezos_ci_jobs.Code_verification.job_build_x86_64_extra_dev
          Schedule_extended_test;
        Tezos_ci_jobs.Code_verification.job_build_kernels Schedule_extended_test;
      ]
    [
      (Auto, job_build_evm_node_static Amd64);
      (Auto, job_build_evm_node_static Arm64);
      (Auto, job_lint_wasm_runtime);
      (Auto, job_unit_tests);
      (Auto, job_test_kernel Schedule_extended_test);
      (Auto, job_test_firehose Schedule_extended_test);
      (Auto, job_test_evm_compatibility Before_merging);
      (Auto, job_test_revm_compatibility Before_merging);
      (Auto, job_tezt `scheduled);
      (Auto, job_tezt_slow `scheduled);
      (Auto, job_tezt_extra `scheduled);
      (Auto, job_tezt_flaky `scheduled);
    ] ;
  ()
