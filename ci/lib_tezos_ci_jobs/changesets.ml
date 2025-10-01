open Tezos_ci

(** {2 Changesets} *)

(** Modifying these files will unconditionally execute all conditional jobs.

        If the CI configuration of [before_merging] or [merge_train]
        pipelines change, we execute all jobs of these merge request
        pipelines. (We cannot currently have a finer grain and run only
        the jobs that are modified.)

        As Changesets should only be present in merge request pipelines,
        other pipelines' files need not be in the changeset.

        [changeset_base] should be included in all Changesets below, any
        exceptions should be explained. *)
let changeset_base =
  Changeset.make
    [
      ".gitlab/ci/pipelines/merge_train.yml";
      ".gitlab/ci/pipelines/before_merging.yml";
      ".gitlab-ci.yml";
    ]

let changeset_images_rust_toolchain =
  Changeset.make
    [
      "images/rust-toolchain/**/*";
      "images/create_image.sh";
      "images/scripts/install_datadog_static.sh";
      "scripts/version.sh";
    ]

let changeset_images_rust_sdk_bindings =
  Changeset.make
    [
      "images/rust-sdk-bindings/**/*";
      "images/create_image.sh";
      "images/scripts/install_datadog_static.sh";
      "scripts/version.sh";
    ]

let changeset_images = Changeset.make ["images/**/*"]

let changeset_images_clientlibs =
  Changeset.make ["images/client-libs-dependencies/**/*"]

let changeset_base_images =
  Changeset.make ["images/base-images/**/*"; "scripts/ci/build-base-images.sh"]

(** Only if octez source code has changed *)
let changeset_octez =
  let octez_source_content =
    List.map
      (fun path -> if Sys.is_directory path then path ^ "/**/*" else path)
      (read_lines_from_file "script-inputs/octez-source-content")
    |> List.filter (fun f -> f <> "CHANGES.rst" && f <> "LICENSES/**/*")
    |> Changeset.make
  in
  Changeset.(
    changeset_base @ octez_source_content
    @ make
        [
          "etherlink/**/*";
          "michelson_test_scripts/**/*";
          "tzt_reference_test_suite/**/*";
        ])

(** Only if octez source code has changed, if the images has changed or
        if kernels.mk changed. *)
let changeset_octez_or_kernels =
  Changeset.(
    changeset_base @ changeset_octez @ changeset_images
    @ make ["scripts/ci/**/*"; "kernels.mk"; "etherlink.mk"])

(** Only if documentation has changed *)

let octez_docs_base_folders =
  [
    "src";
    "tezt";
    "brassaia";
    "irmin";
    "client-libs";
    "etherlink";
    "data-encoding";
    "vendors";
  ]

let changeset_octez_docs =
  Changeset.(
    changeset_base
    (* TODO refine scripts *)
    @ make ["scripts/**/*/"; "script-inputs/**/*/"]
    @ make
        (octez_docs_base_folders |> List.map (fun x -> String.cat x "/**/*.ml*"))
    @ make
        [
          "dune";
          "dune-project";
          "dune-workspace";
          "**/*.rst";
          (* Nota: stays as it is, many non-rst files in this folder *)
          "docs/**/*";
          "grafazos/doc/**/*";
        ])

(** Only if reStructured Text files have changed *)
let changeset_octez_docs_rst = Changeset.(changeset_base @ make ["**/*.rst"])

(* Job [documentation:manuals] requires the build jobs, because it needs
       to run Octez executables to generate the man pages.
       So the build jobs need to be included if the documentation changes. *)
let changeset_octez_or_doc = Changeset.(changeset_octez @ changeset_octez_docs)

let changeset_octez_or_kernels_or_doc =
  Changeset.(changeset_octez_or_kernels @ changeset_octez_docs)

let changeset_octez_docker_changes_or_master =
  Changeset.(
    changeset_base
    @ make
        [
          "scripts/**/*";
          "script-inputs/**/*";
          "src/**/*";
          "tezt/**/*";
          "vendors/**/*";
          "dune";
          "dune-project";
          "dune-workspace";
          "opam";
          "Makefile";
          "kernels.mk";
          "build.Dockerfile";
          "Dockerfile";
        ])

let changeset_docker_files = Changeset.make ["build.Dockerfile"; "Dockerfile"]

let changeset_debian_packages =
  Changeset.(
    make
      [
        ".gitlab/ci/pipelines/debian_repository_partial_auto.yml";
        "scripts/packaging/build-deb-local.sh";
        "scripts/packaging/Release.conf";
        "scripts/packaging/octez/debian/*";
        "debian-deps-build.Dockerfile";
        "scripts/ci/build-debian-packages_current.sh";
        "scripts/ci/build-packages-dependencies.sh";
        "scripts/ci/build-debian-packages.sh";
        "scripts/ci/prepare-apt-repo.sh";
        "scripts/ci/create_debian_repo.sh";
        "docs/introduction/install-bin-deb.sh";
        "scripts/version.sh";
        "manifest/**/*.ml*";
      ])

let changeset_rpm_packages =
  Changeset.(
    make
      [
        ".gitlab/ci/pipelines/rpm_repository_partial_auto.yml";
        "scripts/packaging/build-rpm-local.sh";
        "scripts/packaging/octez/rpm/*";
        "scripts/packaging/tests/rpm/*";
        "rpm-deps-build.Dockerfile";
        "scripts/ci/build-packages-dependencies.sh";
        "scripts/ci/build-rpm-packages.sh";
        "scripts/ci/prepare-apt-rpm-repo.sh";
        "scripts/ci/create_rpm_repo.sh";
        "scripts/version.sh";
        "manifest/**/*.ml*";
      ])

let changeset_homebrew =
  Changeset.(
    make
      [
        ".gitlab/ci/pipelines/homebrew_auto.yml";
        "scripts/packaging/test_homebrew_install.sh";
        "scripts/packaging/homebrew_release.sh";
        "scripts/ci/install-gsutil.sh";
        "scripts/packaging/homebrew_install.sh";
        "scripts/packaging/Formula/*";
        "scripts/version.sh";
        "manifest/**/*.ml*";
      ])

(** The set of [changes:] that select opam jobs.

        Note: unlike all other Changesets, this one does not include {!changeset_base}.
        This is to avoid running these costly jobs too often. *)
let changeset_opam_jobs =
  Changeset.(
    make
      [
        "**/dune";
        "**/dune.inc";
        "**/*.dune.inc";
        "**/dune-project";
        "**/dune-workspace";
        "**/*.opam";
        ".gitlab/ci/jobs/packaging/opam:prepare.yml";
        ".gitlab/ci/jobs/packaging/opam_package.yml";
        "manifest/**/*.ml*";
        "scripts/opam-prepare-repo.sh";
        "scripts/version.sh";
      ])

let changeset_kaitai_e2e_files, changeset_kaitai_checks_files =
  (* this is an over approximation considering all scripts used
         in both Changesets, that mainly differ because of the image
         use to run the jobs *)
  let changeset_kaitai =
    Changeset.make
      [
        "scripts/install_build_deps.js.sh";
        "scripts/version.sh";
        "src/**/*";
        "client-libs/*kaitai*/**/*";
        "scripts/ci/datadog_send_job_info.sh";
        "scripts/slim-mode.sh";
      ]
  in
  ( Changeset.(changeset_base @ changeset_images_clientlibs @ changeset_kaitai),
    Changeset.(changeset_base @ changeset_images @ changeset_kaitai) )

(** Set of OCaml files for type checking ([dune build @check]). *)
let changeset_ocaml_check_files =
  Changeset.(
    changeset_base
    @ make ["src/**/*"; "tezt/**/*"; "devtools/**/*"; "**/*.ml"; "**/*.mli"])

let changeset_lift_limits_patch =
  Changeset.(
    changeset_base
    @ make
        [
          "src/bin_tps_evaluation/lift_limits.patch";
          "src/proto_alpha/lib_protocol/main.ml";
        ])

(* The linting job runs over the set of [source_directories]
       defined in [scripts/lint.sh] that must be included here: *)
let changeset_lint_files =
  Changeset.(
    changeset_base
    @ make
        [
          "src/**/*";
          "tezt/**/*";
          "devtools/**/*";
          "scripts/**/*";
          "docs/**/*";
          "contrib/**/*";
          "client-libs/**/*";
          "etherlink/**/*";
        ])

(** Set of Python files. *)
let changeset_python_files =
  Changeset.(changeset_base @ make ["poetry.lock"; "pyproject.toml"; "**/*.py"])

(** Set of Rust files for formatting ([cargo fmt --check]). *)
let changeset_rust_fmt_files = Changeset.(changeset_base @ make ["**/*.rs"])

(** Set of OCaml files for formatting ([dune build @fmt]). *)
let changeset_ocaml_fmt_files =
  Changeset.(changeset_base @ make ["**/.ocamlformat"; "**/*.ml"; "**/*.mli"])

let changeset_semgrep_files =
  Changeset.(
    changeset_base
    @ make ["src/**/*"; "tezt/**/*"; "devtools/**/*"; "scripts/semgrep/**/*"])

(** Set of Jsonnet files for formatting ([jsonnetfmt --test]). *)
let changeset_jsonnet_fmt_files = Changeset.(make ["**/*.jsonnet"])

(* We only need to run the [oc.script:snapshot_alpha_and_link] job if
       protocol Alpha or if the scripts changed. *)
let changeset_script_snapshot_alpha_and_link =
  Changeset.(
    changeset_base
    @ make
        [
          "src/proto_alpha/**/*";
          "scripts/snapshot_alpha_and_link.sh";
          "scripts/snapshot_alpha.sh";
          "scripts/user_activated_upgrade.sh";
        ])

let changeset_script_b58_prefix =
  Changeset.(
    changeset_base
    @ make
        [
          "scripts/b58_prefix/b58_prefix.py";
          "scripts/b58_prefix/test_b58_prefix.py";
        ])

let changeset_test_liquidity_baking_scripts =
  Changeset.(
    changeset_base
    @ make
        [
          "src/**/*";
          "scripts/ci/test_liquidity_baking_scripts.sh";
          "scripts/check-liquidity-baking-scripts.sh";
        ])

let changeset_test_sdk_rust =
  Changeset.(
    changeset_base
    @ changeset_images_rust_toolchain
      (* Run if the [rust-toolchain] image is updated *)
    @ make ["sdk/rust/**/*"])

let changeset_test_sdk_bindings =
  Changeset.(
    changeset_base
    @ changeset_images_rust_sdk_bindings
      (* Run if the [rust-sdk-bindings] image is updated *)
    @ Sdk_bindings_ci.changeset)

let changeset_test_kernels =
  Changeset.(
    changeset_base
    @ changeset_images_rust_toolchain
      (* Run if the [rust-toolchain] image is updated *)
    @ make
        ["kernels.mk"; "src/kernel_*/**/*"; "src/riscv/**/*"; "sdk/rust/**/*"])

let changeset_riscv_kernels_code =
  Changeset.(
    changeset_base
    @ make ["sdk/rust/**/*"; "src/kernel_sdk/**/*"; "src/riscv/**/*"])

let changeset_riscv_kernels =
  Changeset.(
    changeset_riscv_kernels_code @ changeset_images_rust_toolchain
    (* Run if the [rust-toolchain] image is updated *))

let changeset_mir =
  Changeset.(
    changeset_base @ changeset_images (* Run if the test image is updated *)
    @ make ["contrib/mir/**/*"])

let changeset_mir_tzt =
  Changeset.(
    changeset_base @ changeset_images (* Run if the test image is updated *)
    @ make ["contrib/mir/**/*"; "tzt_reference_test_suite/**/*"])
