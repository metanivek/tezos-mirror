# rust dependencies
#
# This image includes
# - rust dependencies
#
# This image is intended for
# - building kernels, testing kernels and building the kernel SDK in the tezos/tezos CI
# - building the EVM kernel distributed in the tezos/tezos Docker images.

FROM debian:sid

SHELL ["/bin/bash", "-euo", "pipefail", "-c"]

ENV LANG='C.UTF-8' LC_ALL='C.UTF-8' TZ='Etc/UTC'

WORKDIR /root

# common packages
# hadolint ignore=DL3008
RUN apt-get update && \
    apt-get install --no-install-recommends -y \
    ca-certificates curl file \
    build-essential \
    git \
    autoconf automake autotools-dev libtool xutils-dev clang cmake \
    sccache \
    musl-tools && \
    rm -rf /var/lib/apt/lists/*

# install toolchain
RUN curl https://sh.rustup.rs --silent --show-error --fail | \
    sh -s -- --default-toolchain stable -y

ENV PATH=/root/.cargo/bin:$PATH

# install rust toolchains and compilation targets
RUN rustup update 1.66.0 1.73.0 1.76.0 1.78.0 1.80.1 1.84.1 1.86.0 nightly-2025-01-30 nightly-2023-12-15 && \
    for tc in $(rustup toolchain list | awk '{print $1}'); do \
        rustup target add --toolchain="$tc" wasm32-unknown-unknown; \
        rustup target add --toolchain="$tc" riscv64gc-unknown-none-elf; \
        rustup target add --toolchain="$tc" riscv64gc-unknown-linux-gnu; \
        rustup target add --toolchain="$tc" riscv64gc-unknown-linux-musl; \
        rustup target add --toolchain="$tc" x86_64-unknown-linux-musl; \
    done && \
    rustup component add --toolchain=nightly-2025-01-30 rustfmt miri rust-src && \
    rustup component add --toolchain=nightly-2023-12-15 rust-src llvm-tools

# Install Rust 1.73.0 standard library for riscv64gc-unknown-hermit
RUN curl -L "https://github.com/hermit-os/rust-std-hermit/releases/download/1.73.0/rust-std-1.73.0-riscv64gc-unknown-hermit.tar.gz" -o rust-std-1.73.0-riscv64gc-unknown-hermit.tar.gz \
    && (echo 65980ab1110081a6b7edd70b45bebe63a50e34a6e1555d6fc000c35360907547 rust-std-1.73.0-riscv64gc-unknown-hermit.tar.gz | sha256sum -c) \
    && tar xf rust-std-1.73.0-riscv64gc-unknown-hermit.tar.gz \
    && rust-std-1.73.0-riscv64gc-unknown-hermit/install.sh \
    && rm -r rust-std-1.73.0-riscv64gc-unknown-hermit rust-std-1.73.0-riscv64gc-unknown-hermit.tar.gz

# Install Rust 1.78.0 standard library for riscv64gc-unknown-hermit
RUN curl -L "https://github.com/hermit-os/rust-std-hermit/releases/download/1.78.0/rust-std-1.78.0-riscv64gc-unknown-hermit.tar.gz" -o rust-std-1.78.0-riscv64gc-unknown-hermit.tar.gz \
    && (echo a4454f71a021b7af62f00b527cd6da28832e93728b606f6c4964c6c2c04789f0 rust-std-1.78.0-riscv64gc-unknown-hermit.tar.gz | sha256sum -c) \
    && tar xf rust-std-1.78.0-riscv64gc-unknown-hermit.tar.gz \
    && rust-std-1.78.0-riscv64gc-unknown-hermit/install.sh \
    && rm -r rust-std-1.78.0-riscv64gc-unknown-hermit rust-std-1.78.0-riscv64gc-unknown-hermit.tar.gz

# Install Rust 1.80.1 standard library for riscv64gc-unknown-hermit
RUN curl -L "https://github.com/hermit-os/rust-std-hermit/releases/download/1.80.1/rust-std-1.80.1-riscv64gc-unknown-hermit.tar.gz" -o rust-std-1.80.1-riscv64gc-unknown-hermit.tar.gz \
    && (echo 9fc4e10aa629c87fd3aa09ffee57349d7d354364dbe8e0a128ebd94ccb30bac7 rust-std-1.80.1-riscv64gc-unknown-hermit.tar.gz | sha256sum -c) \
    && tar xf rust-std-1.80.1-riscv64gc-unknown-hermit.tar.gz \
    && rust-std-1.80.1-riscv64gc-unknown-hermit/install.sh \
    && rm -r rust-std-1.80.1-riscv64gc-unknown-hermit rust-std-1.80.1-riscv64gc-unknown-hermit.tar.gz

# Install Taplo
RUN cargo +1.78.0 install taplo-cli --version 0.8.1 --locked && \
    taplo --version

# Install cargo-audit
RUN cargo +1.78.0 install --locked cargo-audit --version 0.20.0 && \
    cargo audit --version

# install wabt: https://packages.debian.org/source/sid/wabt
# Install a version of binutils which supports RISC-V and put symlink in PATH
# hadolint ignore=DL3008
RUN apt-get update \
    && apt-get install --no-install-recommends -y wabt binutils-riscv64-unknown-elf gcc-riscv64-linux-gnu \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* \
    && ln -s /usr/bin/riscv64-linux-gnu-gcc /usr/bin/riscv64-unknown-linux-musl-gcc 

RUN ln -s /usr/bin/riscv64-unknown-elf-objdump /usr/local/bin/objdump
