# SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
# SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
#
# SPDX-License-Identifier: MIT

.PHONY: all
all: build test check

# In some situations we might want to override the compilation target.
NATIVE_TARGET ?=

ifneq ($(NATIVE_TARGET),)
NATIVE_OPT := --target="$(NATIVE_TARGET)"
endif

# We don't want to check all features in CI, so we provide this varible to configure it.
CHECK_FEATURES ?= --all-features

.PHONY: build
build: riscv-dummy.elf riscv-dummy-sdk.elf riscv-sandbox
	@cargo build --release --workspace $(NATIVE_OPT)
	@make -C jstz build
	@make -C tests build

.PHONY: riscv-sandbox
riscv-sandbox::
	@cargo build --release --package riscv-sandbox $(NATIVE_OPT)
	@ln -f target/$(NATIVE_TARGET)/release/riscv-sandbox $@

.PHONY: riscv-sandbox.prof
riscv-sandbox.prof::
	@cargo build --profile profiling --package riscv-sandbox
	@ln -f target/profiling/riscv-sandbox $@

riscv-dummy.elf::
	@make -C dummy_kernel build
	@ln -f dummy_kernel/target/riscv64gc-unknown-hermit/release/riscv-dummy $@

riscv-supervised-dummy.elf::
	@make -C supervised_dummy_kernel build
	@ln -f supervised_dummy_kernel/target/riscv64gc-unknown-linux-musl/debug/riscv-supervised-dummy $@

riscv-dummy-sdk.elf::
	@make -C dummy_sdk_kernel build
	@ln -f dummy_sdk_kernel/target/riscv64gc-unknown-hermit/release/riscv-dummy-sdk $@

# Update checked-in binaries
.PHONY: update-assets
update-assets: assets/riscv-dummy.elf assets/jstz

# Update the compiled dummy kernel used in CI tests
assets/riscv-dummy.elf assets/riscv-dummy.elf.checksum::
	@make -C dummy_kernel build
	@cp dummy_kernel/target/riscv64gc-unknown-hermit/release/riscv-dummy $@
	@sha256sum $@ > $@.checksum

assets/jstz assets/jstz.checksum::
	@make -C jstz build
	@cp jstz/target/riscv64gc-unknown-hermit/release/jstz $@
	@sha256sum $@ > $@.checksum

.PHONY: build-deps
build-deps:
	@make -C jstz build-deps
	@make -C dummy_kernel build-deps
	@make -C supervised_dummy_kernel build-deps
	@make -C dummy_sdk_kernel build-deps
	@rustup component add rustfmt clippy
ifneq ($(NATIVE_TARGET),)
	@rustup target add $(NATIVE_TARGET)
endif

	# Iterate through all the toolchains. 'rustup show' will install the
	# toolchain in addition to showing toolchain information.
	@find . -iname 'rust-toolchain*' -execdir rustup show active-toolchain \; 2>/dev/null

.PHONY: test
test: build
	@cargo test --workspace
	@make -C dummy_kernel test
	@make -C jstz test

.PHONY: test-long
test-long: 
	@cargo test --release -- test_jstz_determinism --nocapture --ignored

.PHONY: check
check:
	# Check format of all crates
	@exec ../../scripts/check-format-rust.sh

	@cargo update --workspace --locked
	@cargo check --workspace --all-targets $(CHECK_FEATURES)
	@cargo clippy --workspace --all-targets $(CHECK_FEATURES) -- --deny warnings
	@cargo doc --document-private-items --no-deps $(CHECK_FEATURES)
	@taplo format --check $(shell git ls-files '**/Cargo.toml')
	@make -C dummy_kernel check
	@make -C supervised_dummy_kernel check
	@make -C dummy_sdk_kernel check
	@make -C jstz check
	@make -C tests check

.PHONY: audit
audit:
	@# We don't add this to the check target because it requires installation
	@# of 'cargo-audit' via 'cargo install' which we can't control very well
	@# on the machines of other developers. E.g. if we need to upgrade its
	@# version, it may cause issues for developers.
	@# For that reason we expose this as a separate target and run it only in
	@# CI.
	@cargo audit

.PHONY: clean
clean:
	@cargo clean
	@make -C dummy_kernel clean
	@make -C supervised_dummy_kernel clean
	@make -C dummy_sdk_kernel clean
	@make -C jstz clean
	@make -C tests clean
	@rm -f riscv-sandbox riscv-dummy.elf riscv-dummy-sdk.elf
