# SPDX-FileCopyrightText: 2023-2024 TriliTech <contact@trili.tech>
#
# SPDX-License-Identifier: MIT

NATIVE_TARGET ?= $(shell rustc -vV | grep 'host:' | awk '{print $$2}')
ifneq ($(NATIVE_TARGET),)
NATIVE_OPT := --target "$(NATIVE_TARGET)"
endif

INBOX_FILE ?= $(shell mktemp)

.PHONY: all
all: build test check inbox-bench

.PHONY: build-deps
build-deps:
	@exec ../scripts/install-rust-std-hermit.sh
	@rustup component add rustfmt clippy
ifneq ($(NATIVE_TARGET),)
	@rustup target add $(NATIVE_TARGET)
endif

.PHONY: build
build: build-kernel inbox-bench

.PHONY: build-kernel
build-kernel:
	@cargo build -p jstz --release

.PHONY: build-kernel-static
build-kernel-static:
	@INBOX_FILE=$(INBOX_FILE) cargo build -p jstz --release --features static-inbox

.PHONY: test
test:
	@cargo test --no-default-features $(NATIVE_OPT)
	@../scripts/jstz-bench.sh -t 1
	@../scripts/jstz-bench.sh -t 1 -s

.PHONY: run
run:
	@cargo run --no-default-features

.PHONY: check
check:
	@INBOX_FILE=$(INBOX_FILE) cargo check --all-features --workspace --locked
	@INBOX_FILE=$(INBOX_FILE) cargo clippy --all-features -- --deny warnings

.PHONY: inbox-bench
inbox-bench:
	@cargo build $(NATIVE_OPT) --bin inbox-bench --release
	@cp target/$(NATIVE_TARGET)/release/inbox-bench .

.PHONY: clean
clean:
	@cargo clean
