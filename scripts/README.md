This directory contains various utility scripts related to the project lifecycle.

# Overview

In particular, this includes scripts for:
* installing the project environment (`install_*.sh`)
* setting up environment variables for building or using Tezos (`env.sh`)
* generating the Docker image used by the CI (`create_docker_image.sh`, `ci/`)
  or used by users and developers (`docker/`)
* dune files instrumentation for computing test coverage (`instrument_dune_bisect.sh`)
* maintaining a custom opam repository with all Tezos dependencies
 (`update_opam_repo.sh`, `opam-*.sh`)
* generate base58 prefixes (`b58_prefix/`)
* generate configuration code (in OCaml) to bootstrap a new network (`gen-genesis/`)
* preparing the environment for migration tests (`prepare_migration_test.ml`)
* releasing static Octez binaries (`release/`)
* snapshotting protocols, linking them to the build system (`snapshot_alpha.sh`, `link_protocol.sh`, `snapshot_alpha_and_link.sh`)
* getting baker version stats from a range of blocks (`baker-version-stats.sh`)
* printing a dependency tree based on ocamldep output (`ocamldot.py`)
* creating a yes-wallet, to be used in conjunction with a yes-node (`yes-wallet/` and `yes-node.patch`)
* setting user-activated upgrades (`user_activated_upgrade.sh`)
* generating commitments (initial accounts) for test networks (`create_genesis/create_genesis_info.py`)
* patch the protocols to add profiling (`patch-profiler-proto.sh`)

This directory also includes an example docker-compose file to run a
node with a baker and an accuser (`docker/docker-compose-generic.yml`)
and a minimalistic Michelson REPL built on top of `octez-client`
(`michelson_repl.sh`).

## API
<!-- For each script where this is possible, indicate how to obtain usage info
  (e.g. invoke with no args; or link to doc page where the tool is explained), and
  maybe an example of use. -->

Information about using some of the scripts above can be found as follows:

* `instrument_dune_bisect.sh`: See <https://tezos.gitlab.io/developer/testing.html>.
* `update_opam_repo.sh`: see <https://tezos.gitlab.io/developer/contributing-adding-a-new-opam-dependency.html>
* `version.sh`: see <https://tezos.gitlab.io/introduction/howtoget.html> and <https://tezos.gitlab.io/developer/contributing-adding-a-new-opam-dependency.html>
* `yes-wallet/`: see <http://tezos.gitlab.io/developer/proposal_testing.html>
* `user_activated_upgrade.sh`: see <http://tezos.gitlab.io/developer/proposal_testing.html>

## Profiler patches for the protocols

If the profiler patches are conflicting for some reason, the simplest is
basically to regenerate them (assuming the conflicting branch is based on
`master`, otherwise replace it with your branches' upstream):

1. create a local branch on top of master (let's call it `master-patched`),
apply the patch on top of it and create a commit:

```
git apply -- scripts/profile_alpha.patch
git commit -a -m "Patch alpha"
```

2. Move to your working branch, cherry-pick `patch-alpha`, which will put the
commit `Patch alpha` on top of it. If there are conflicts, resolve them.

3. Create a patch out of it and commit it:
```
git format-patch -n HEAD^
git reset --hard HEAD^ # to remove the patched commit, but not the generated file
mv <patch-file> scripts/profiler_alpha.patch
git commit -a -m "Scripts: regenerate protocol alpha patches"
```

4. You can remove the temporary branch `master-patched` for the patch
