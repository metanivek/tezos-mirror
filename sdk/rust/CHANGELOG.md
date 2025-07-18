# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [Unreleased]

### Added

- Add `SecretKeySecp256k1` and `SecretKeyP256` hashes
- Add `EncryptedSecretKeyEd25519`, `EncryptedSecretKeySecp256k1`, `EncryptedSecretKeyP256` and `EncryptedSecretKeyBls` hashes
- Add `ScriptExprHash` hash
- Allow the unit type `()` as field in derived implementations of `NomReader` and `BinWriter`.
- Add a new package `tezos-protocol`, holding the Tezos protocol structures.
- Add `Contract` defining contract address.
- Add `Entrypoint` defining transaction entrypoint.
- Add `OperationContent`, `ManagerOperationContent`, `RevealContent`, `TransactionContent` defining operations contents for reveal and transaction.
- Add `DelegationContent` defining operation content for delegation.
- Add `OriginationContent` defining operation content for origination.

### Changed

- Add `types::Narith`, which wraps a `BigUint`.
- Deprecate `types::Mutez`. It is left as a type-alias to `types::Narith`.
  *NB* The internally wrapped type is changed from `BigInt` to `BigUint` as a result.
- Add `#[encoding]` attribute support for enum fields, in addition to struct fields.
- `tezos_data_encoding_derive`: derivations of `NomReader` and `BinWriter` require those same constraints on their fields. If such a field doesn't meet the constraint, the implementation will not be available for use, however compilation will still succeed.

### Fixed

- Fix `short_dynamic` function in `encoding` - was incorrectly using `dynamic` internally.

### Security

- Nothing.

### Performance

- Nothing.

## [0.6.0 - 2024-07-01]

### Added

- Add `FromBase58CheckError::IncorrectBase58Prefix` variant.
- Add `NomReader`, `BinWriter` support for `Ed25519Signature`.
- Add `signature::Signature` enum representing possible types of signature used in Tezos.
- Add `From<PublicKeyEd25519>` impl for `ContractTz1Hash`.
- Add `From<PublicKeySecp256k1>` impl for `ContractTz2Hash`.
- Add `From<PublicKeyP256>` impl for `ContractTz3Hash`.
- Add `From<PublicKeyBls>` impl for `ContractTz4Hash`.
- Add `TryFrom<Signature>` impl for various signature types.
- Add `PublicKeySignatureVerifier` impl for `PublicKeyBls`.
- Add `PublicKey`, `PublicKeyHash` aggregate types.

### Changed

- `tezos_data_encoding`: The `NomReader` trait is now explicitly
parameterized by the lifetime of the input byte slice.
- Altered hashes to implement `AsRef<[u8]>` instead of `AsRef<Vec<u8>>`.
- Renamed `hash::Signature` to `hash::UnknownSignature`.
- All hash structs no longer publicly expose the underlying `Hash`. Instead, use `Into<Vec<u8>>` and `AsRef<[u8]>`.
- `ToBase58Check` no longer returns a `Result` type.
- `blake2b::digest_128`, `blake2b::digest_160`, `blake2b::digest_256` return
  plain `Vec<u8>` instead of `Result<Vec<u8>, Blake2bError>`, the error was
  never possible.
- `blake2b::merkle_tree` returns plain `Vec<u8>` instead of `Result<Vec<u8>,
  Blake2bError>`, the error was never possible.
- `tezos_crypto_rs`: `PublicKeyWithHash::pk_hash` now returns `Self::Hash`
  instead of `Result`.
- `PublicKeySignatureVerifier` now requires the explicitly correct signature kind for the given public key.
- Update `num-bigint` dependency to `0.4` to improve WASM compatibility.
- Minimum supported rust version bumped to `1.64`.
- `tezos_crypto_rs` now depends on `tezos_data_encoding`, rather than vice-versa.

### Deprecated

- Nothing.

### Removed

- Removed legacy `SecretKeyEd25519` encoding.
- Removed `ToBase58CheckError`.
- Removed unused `Blake2bError::Other`.
- Removed impossible `TryFromPKError`.
- `tezos_data_encoding`: Removed unused `DecodeErrorKind::Hash` and
  `DecodeError::hash_error`
- `tezos_crypto_rs`: Removed unused `Error` type from `PublicKeyWithHash`
- Removed support for `hash` field attribute when deriving encodings.

### Fixed

- Fix prefix used in `SeedEd25519` encoding.
- Add explicit prefix check during base58check decoding.
- Hash input before signing with `SecretKeyEd25519`, to match octez impl.
- Fix `BlsSignature` base58 check encoding/decoding.
- Fix `SecretKeyEd25519` base58 check encoding/decoding.
- Fix all zeros signature encoding: should be `Unknown` rather than defaulting to `Ed25519`.
- Fix `tz1` signature verification: input should be hashed.
- Fix `tz2` signature verification: input should be hashed.
- Fix `tz3` signature verification: input should be hashed.

### Security

- Bump curve25519-dalek to 4.1.3 to address RUSTSEC audit issue.

### Performance

- Nothing.

## [0.5.2] - 2023-11-21

### Added

- Added `bls` feature flag to `tezos_crypto_rs`, to allow disabling dependency on `blst`.

## [0.5.1] - 2023-09-01

### Added

- Nothing.

### Changed

- Fixed the version of `blst` to 0.3.10
- Set the version of `ed25519-dalek` to 2.0.0

## [0.5.0] - 2023-05-12

### Added

- Nothing.

### Changed

- Ed25519 implementation switched from `ed25519_compact` to `ed25519-dalek`.
- `PublicKeyEd25519::sign` no longer takes an `Iterator`, instead only one `AsRef<u8>` is allowed.
- Depencies bumped, including `nom` -> `7.1`.

### Deprecated

- Nothing.

### Removed

- `tezos_crypto_rs`: errors no longer implement `PartialEq`, `Clone`.
- `tezos_crypto_rs`: errors no longer are (de)serializable with serde.

### Fixed

- Nothing.

### Security

- Nothing.

### Performance

- Improvements in verification of `tz1` signatures, due to change in Ed25519 backend.

## [0.4.4] - 2023-03-16

### Fixed

- `tezos_data_encoding_derive` was outputting code that referenced the old `tezos_encoding` crate. It now references `tezos_data_encoding`.

## [0.4.3] - 2023-03-14

## [0.4.2] - 2023-03-14

### Changed

- `tezos_crypto` rename to `tezos_crypto_rs`, due to conflict on crates.io

## [0.4.1] - 2023-03-14

### Changed

- `tezos_encoding` renamed to `tezos_data_encoding`
- `tezos_encoding_derive` renamed to `tezos_data_encoding_derive`

## [0.4.0] - 2023-03-13

### Added

- support `tz4`, `sr1` hashes.
- `SecretKeyBls`, `PublicKeyBls` support.

### Changed

- minimum supported rust version bumped to `1.60`.
- `SecretKeyEd25519` now implements `Debug`, and can be encoded/decoded.
- `crypto` renamed to `tezos_crypto`.
- `tezos_encoding_derive` now supports deriving over generic structs.
- `tezos_encoding` `nom::list`, `nom::dynamic` no longer require `Clone`.
- `lib_sodium` dependency replaced with `cryptoxide` for improved Wasm support.

### Removed

- All crates, except for `crypto`, `tezos_encoding` & `tezos_encoding_derive`.
- `crypto::seeded_step`, `crypto::cryptobox` modules removed.

## [3.1.1] - 2022-06-28

### Fixed

- Added missing jakarta protocol recognition to context timings tool.

## [3.1.0] - 2022-06-27

### Added
- Jakarta support in rewards RPC.

### Fixed

- Fixed rewards RPC edge cases when the interrogated cycle was lower than preserved_cycles.
- Fixed embedded baker for blocks with predecessor containing a protocol activation.
- Minor fixes in Tezos encoding library.

### Security

- Removed `chrono` dependency from `node_monitoring` application.

## [3.0.0] - 2022-06-25

### Added
- Internal baker built inside the node.
- Prechecking for manager operations.
- Jakarta support in mempool and bakers.

### Changed

- New version of block application code in the protocol runner that performs extra checks.
- Prechecking for endorsement operations is now disabled by default.

## [2.3.6] - 2022-06-16

### Changed

- Don't disconnect peers when in private mode.
- Include `tezedge-baker` program in docker image.

## [2.3.5] - 2022-06-16

### Fixed

- Baker live_block filter.

## [2.3.4] - 2022-06-15

### Added

- Double-baking and double-endorsement protection in baker
- Updated documentation.

## [2.3.3] - 2022-06-10

### Fixed

- Fixed possible errors while calling RPCs due to the WebSocket RPC feature.
- Fixed handling of lazy encoded micheline values in operations.
- Fixed unimplemented panic in describe endpoint.

### Performance

- Split the rewards RPC into two (/dev/rewards/cycle/:cycle_num and /dev/rewards/cycle/:cycle_num/:delegate) to further optimise performance

## [2.3.2] - 2022-06-07

### Fixed

- In the baker, filter out outdated operations when injectiong a block.
- Better handling of injection RPC responses for unparseable operations.

## [2.3.1] - 2022-06-06

### Added

- New baker documentation.

### Fixed

- Handle corner case when recovering the context storage from a hard crash.
- Add missing flush call for the chain storage after block application to ensure it can be recovered from a hard crash.
- Bug in baker scheduler that caused some requests to wrongly eclipse previous ones.

## [2.3.0] - 2022-06-03

### Added

- Preliminary support for protocol 013-Jakarta.
- Added a baker implementation.
- Added RPC for baker rewards distribution.
- Added support for tezedge RPCs through websocket using JSON-RPC 2.0.

### Changed

- Changed the monitoring websocket communication from a push based architecture to a request/response architecture.
- Reworked the prevalidated code in the protocol runner.

### Fixed

- `/chains/:chain/blocks` RPC.

## [2.2.0] - 2022-04-29

### Added

- In-memory context backend now regularly takes a snapshot of the most recent commits,
  and is able to restore its state when the node is restarted.
- Prevalidation priority logic, so that injected and consensus operations
  get validated first to increase the chance of them being included
  in the next block.
- Better and more detailed action statistics: https://tezedge.com/#/resources/state
- Slack alerts for missed block/endorsement for a specific baker.
- Report generation in case of missed block/endorsement, which includes
  debug information.

### Fixed

- Issues with mempool and prevalidator.
- Assertion failure in tezedge native context storage when running ithacanet.
- Rendering of operations metadata JSON in RPCs when there is too much data

### Performance

- Optimized block application to avoid reapplying block in case of branch change.

## [2.1.0] - 2022-04-06

### Added

- Support for preendorsements UI.
- Bounds for operations in mempool.

### Fixed

- Fixed issue with node being disconnected from all peers.
- Fixed support for Ithaca endorsements UI.
- Fixed wrong duration in action statistics.

## [2.0.0] - 2022-03-31

### Changed

- Adapt bootstrapping and mempool logic for Ithaca.
- State machine fuzzing improvements.

### Fixed

- On injection RPCs, respect the `is_async` parameter and respond without waiting for the injection to complete.
- Fixed sporadic connectivity issues between the shell and protocol runner processes.

## [1.19.0] - 2022-03-25

### Added

- Import snapshot subcommand for importing snapshots from a remote file server.

### Changed

- The --config-file argument now defaults to the default config file.

## [1.18.1] - 2022-03-22

### Changed

- For macOS-arm64, use an updated libtezos.

## [1.18.0] - 2022-03-22

### Added

- Support for macOS-arm64 (M1 CPUs).
- Block headers download progress logging.

## [1.17.0] - 2022-03-18

### Added

- Preliminary support for protocol 012 (Ithaca 2).
- Re-implemented bootstrapping logic in the state machine.
- In-memory context context now supports loading it's state from disk.
- Disk usage measurements for the context stats db.
- Implemented Octez `tezos-accuser` support.

### Changed

- In-memory context storage now has a new garbage collector that collects everything that doesn't belong to the last few levels.
- Block application retry logic is now more alike Octez, and can handle protocol runner failures.

### Fixed

- Fixed `/monitoring/valid_blocks` RPC.
- Cache-related block application retry logic now in synch with the one in Octez.

## [1.16.1] - 2022-03-04

### Changed

- Rust 2021 edition is used.
- The option `--disable-apply-retry` now is `false` by default.

### Fixed

- Fixed `/chains/:chain-id:/blocks` RPC that prevented seed nonces to be revealed.
- Fixed potential node stucking when using `--disable-apply-retry=true`.
- Fixed issue that caused the snapshot command to sometimes timeout on slower computers.

### Security

- RUSTSEC-2020-0071 is fixed by using newer `time` crate version.
- RUSTSEC-2020-0159 is fixed by using `time` crate instead of `chrono`.

## [1.16.0] - 2022-02-28

### Added

- New `snapshot` subcommand to `light-node` to produce trimmed copies of the storage.
- Option to enable retrying block application with cache reloaded.
- Implemented shell-side prechecker for endorsement operations.
- Implemented optional shell-side block prechecking.
- Implemented statistics RPCs for tracking new blocks processing.
- Re-implemented block application logic in state machine.
- Re-implemented protocol-runner subprocess handling in the state machine.

### Changed

- Rust upgraded to 1.58.1

### Fixed

- Missed endorsement when alternative heads are encountered.
- The `/monitor/bootstrapped` RPC now properly reports bootstrapping progress if the  node is not bootstrapped already.

## [1.15.1] - 2022-02-18

### Fixed

- Bug when mapping OCaml->Rust values representing error responses from protocol RPCs.

## [1.15.0] - 2022-02-18

### Added

- Various tools for the Rust implementation of the context storage (see `tezos/context-tool`).
- `context-integrity-check` flag to check the integrity of the Rust implementatino of the context storage at startup.

### Changed

- Released binaries no longer make use of ADX instructions, increasing comptability with more CPUs.

### Performance

- Improved the representation of the context storage inodes so that less memory is used.

## [1.14.0] - 2021-12-24

### Fixed

- In Rust implementation of persisted context storage, commits now
  behave as an atomic operations.

### Changed

- Switch default Rust toolchain to stable **Rust 1.57** version.
- Rewrote and moved mempool implementation from actor system to new state
  machine architecture.

### Added

- Compatibility with Hangzhou protocol.
- Rpc to get accumulated mempool operation statistics.

### Performance

- Optimized Rust implementation of persisted context storage.

## [1.13.0] - 2021-12-01

### Fixed

- Removed redundant validations of `current_head` from peers, which in some cases was causing the node to lag behind.

## [1.12.0] - 2021-11-30

### Changed

- Increase Irmin's index log size limit to `2_500_000` to match Octez v11. This should help with freezes during merges by making them happen less often, but increases memory usage a little.
- Tweak the call to apply a block in chain feeder so that it is less prone to block Tokio's worker threads.
- MessagePack encoding is now used for action and state snapshot storage instead of JSON.

## [1.11.0] - 2021-11-29

### Added

- Persistent on-disk backend for the TezEdge context storage (preview release)
- Enabling conditions for actions in shell automaton.
- Drone CI Pipeline with Python tests for Granada
- Per-protocol context statistics

### Removed

- Drone CI pipeline with Python tests for Edo

### Changed

- When using the TezEdge context storage implementation, the default backend is now the persistent one (was in-memory)
- More eager cleanup of no-longer used IPC connections between the shell process and the protocol runner process, and more reuse of already existing connections (instead of instantiation a new one each time) when possible.

### Fixed

- Issue that caused the list of peers between the state machine and the actors system to get out of sync, causing the node to lag behind.

## [1.10.0] - 2021-11-16

### Changed

- Rewrote P2P networking and peer management to new architecture.
- Made IPC communication with the protocol runner processes asynchronous.
- Renamed cli argument `--disable-peer-blacklist` to `--disable-peer-graylist`.

### Deprecated

- Synchronous ocaml IPC.

### Removed

- Actor based P2P networking and peer management.
- FFI connection pool flags:
  - `--ffi-pool-max-connections`
  - `--ffi-trpap-pool-max-connections`
  - `--ffi-twcap-pool-max-connections`
  - `--ffi-pool-connection-timeout-in-secs`
  - `--ffi-trpap-pool-connection-timeout-in-secs`
  - `--ffi-twcap-pool-connection-timeout-in-secs`
  - `--ffi-pool-max-lifetime-in-secs`
  - `--ffi-trpap-pool-max-lifetime-in-secs`
  - `--ffi-twcap-pool-max-lifetime-in-secs`
  - `--ffi-pool-idle-timeout-in-secs`
  - `--ffi-trpap-pool-idle-timeout-in-secs`
  - `--ffi-twcap-pool-idle-timeout-in-secs`

## [1.9.1] - 2021-11-04

### Fixed

- Optimized mempool operations downloading from p2p

## [1.9.0] - 2021-10-26

### Added

- Support for Ubuntu 21

### Changed

- Shell refactor and simplify communication between actors
- Upgrade to Tokio 1.12
- `riker` dependency replaced with `tezedge-actor-system` dependency

### Removed

- Removed `riker` dependency from `rpc` module

### Fixed

- Optimize `chains/{chain_id}/mempool/monitor_operations` and `monitor/heads/{chain_id}` RPCs.
- Controlled startup for chain_manager - run p2p only after ChainManager is subscribed to NetworkChannel
- ChainFeeder block application improved error handling with retry policy on protocol-runner restart
- Added set_size_hint after decoding read_message to avoid unnecessary recounting for websocket monitoring

## [1.8.0] - 2021-09-20

### Added

- Added new, faster implementation of binary encoding of p2p messages
- Added encoding benchmarks
- Added new context storage optimization that takes advantage of repeated directory structures
- Added build file cache to CI
- Added new, faster implementation for endorsing and baking rights RPCs that use cached snapshots data
- Added handlers for protocol 009 and 010 baking and endorsing rights RPC.

### Changed

- Changed historic protocols to use new storages for baking/endorsing rights calculation.
- Internal cleanup of the context storage implementation + documentation
- Replaced use of the failure crate with thiserror + anyhow crates

## [1.7.1] - 2021-08-31

### Fixed

- Fix a corner case in the communication with the protocol runner that could cause some off-by-one responses.

## [1.7.0] - 2021-08-19

### Added

- Bootstrap time test to Drone CI
- Implemented inodes representation in the TezEdge context storage

### Changed

- Upgrade rust nightly to 2021-08-04
- Drone CI test pipelines optimization and improvements
- Drone CI test pipelines optimization and improvements

### Fixed

- Be more robust on the handling of IPC errors
- Calculate block header hash on decoding optimization
- TezEdge context fixes, optimizations and improvements
- Fixed chrono dependency panics
- Reworked <block_id> parsing for rpc and better error handling (future blocks, ...)

### Removed

- Remove no-longer used COPY context function

## [1.6.10] - 2021-07-30

### Changed

- New irmin context version

## [1.6.9] - 2021-07-22

### Changed

- RPC uses protocol runners without context for decoding block/operations metadata

## [1.6.8] - 2021-07-20

### Fixed

- Add dns/resolv libs to distroless docker image

## [1.6.7] - 2021-07-20

### Added

- Quota throttling for p2p messages

## [1.6.6] - 2021-07-16

### Added

- Add massif test for bootstrapping

### Changed

- Upgrade tokio dependency to 1.8

### Fixed

- Cleaning and better error handling

## [1.6.5] - 2021-07-13

### Added

- Add support for custom networks specified by a config file
- Log configuration on startup

### Fixed

- Storage db compatibility for 19 vs 20 for snapshots

## [1.6.4] - 2021-07-12

### Changed

- Slog default rolling parameters to save more logs

## [1.6.2] - 2021-07-07

### Fixed

- Block storage iterator was returning blocks in reverse order.

## [1.6.1] - 2021-07-07

### Added

- A reworked in-memory backend for the TezEdge context that conforms to the Tezos context API and is now directly accessed from the Tezos protocol code.
- Flag `--tezos-context-storage` to choose the context backend. Default is `irmin`, supported values are:
  - `tezedge` - Use the TezEdge context backend.
  - `irmin` - Use the Irmin context backend.
  - `both` - Use both backends at the same time
- `inmem-gc` option to the `--context-kv-store` flag.
- Flag `--context-stats-db-path=<PATH>` that enables the context storage stats. When this option is enabled, the node will measure the time it takes to complete each context query. When available, these will be rendered in the TezEdge explorer UI.
- A new `replay` subcommand to the `light-node` program. This subcommand will take as input a range of blocks, a blocks store and re-apply all those blocks to the context store and validate the results.
- A new CI runner running on linux with real-time patch kernel to increase determinism of performance tests
- Add conseil and tzkt tests for florencenet
- Add caching to functions used by RPC handlers

### Changed

- Implemented new storage based on sled as replacement for rocksdb
- Implemented new commit log as storage for plain bytes/json data for block_headers
- New dockerhub: simplestakingcom -> tezedge

### Removed

- Flag `--one-context` was removed, now all context backends are accessed directly by the protocol runner.
- RocksDB and Sled backends are not supported anymore by the TezEdge context.
- The actions store enabled by `--actions-store-backend` is currently disabled and will not record anything.

## [1.5.1] - 2021-06-08

### Added

- Preserve frame pointer configuration (used by eBPF memprof docker image)

### Changed

- Updated docker-composes + README + sandbox update for 009/010

## [1.5.0] - 2021-06-06

### Added

- New protocol 009_Florence/010_Granada (baker/endorser) integration support + (p2p, protocols, rpc, tests, CI pipelines)

### Changed

- Encodings - implemented NOM decoding
- (FFI) Compatibility with Tezos v9-release
- Store apply block results (header/operations) metadata as plain bytes and added rpc decoding to speedup block application
- TezEdge node now works just with one Irmin context (temporary solution, custom context is coming soon...)

## [1.4.0] - 2021-05-20

### Changed
- Optimize the Staging Area Tree part of Context Storage
- Shell memory optimizations
- Changed bootstrap current_branch/head algo
- New Drone CI with parallel runs

### Security

- Added Proof-of-work checking to hand-shake

## [1.3.1] - 2021-04-14

### Added

- New module `deploy_monitoring` for provisioning of TezEdge node, which runs as docker image with TezEdge Debugger and TezEdge Explorer
- Flag `--one-context` to turn-off TezEdge second context and use just one in the FFI
- Peer manager stats to log
- More tests to networking layer

### Fixed
- P2p manager limit incoming connections by ticketing
- Dead-lettered peer actors cleanup
- Memory RAM optimization

## [1.2.0] - 2021-03-26

### Added

- Automatically generated documentation on P2P messages encoding.
- Context actions record/replay feature
- Flag `--actions-store-backend <BACKEND1> <BACKEND2> ...`. When enabled the node stores incomming actions in one of the selected backends. Possible values are: `rocksdb`, `file`
- Flag `--context-kv-store=STRING`. Chooses backend for data related to merkle storage. By default rocksdb database is used, possible values are :
  - `rocksdb` - persistent [RocksDB](https://rocksdb.org/) database
  - `sled` - persistent [Sled](http://sled.rs) database
  - `inmem` - volatile in memory database(unordered)
  - `btree` - volatile in memory database(ordered)
- Added new RPCs for get operations details

### Changed

- Storage module refactor
- Upgrade code to ocaml-interop v0.7.2

### Security

- Safer handling of String encoding

## [1.1.4] - 2021-03-12

### Added

- Extended tests for calculation of context_hash
- Possibility to use multiple logger (terminal and/or file)

### Changed

- Shell refactor to batch block application to context

## [1.1.3] - 2021-03-09

### Fixed

- Correct parsing of bootstrap addresses with port

### Deprecated

- edo/edonet - automatically is switched to edo2/edo2net

## [1.1.2] - 2021-03-05

### Added

- New 008 edo2 support + possibility to connect to edo2net
- New algorithm for calculation of context_hash according to Tezos

## [1.1.1] - 2021-03-05

### Fixed
- README.md and predefined docker composes

## [1.1.0] - 2021-03-02

### Added

- Sapling zcash-params init configuration handling for edo protocol on startup
- Backtracking support for Merkle storage

### Changed

- Argument `--network=` is required + possibility to run dockers with different networks
- RPC integration tests optimization - run in parallel and add elapsed time to the result in Drone CI
- Minor changes for dev RPCs for TezEdge-Explorer

### Removed

- Argument `--ffi-calls-gc-treshold`.
- Default value for argument `--network=`

### Fixed

- Used hash instead of string for peer_id in SwapMessage

### Security

- Added limits for p2p messages according to the Tezos updates

## [1.0.0] - 2021-02-10

### Added

- Rpc for protocol runner memory stats

### Changed

- Tokio update to 1.2.x version.
- Shell and bootstrap refactored to use kind of bootstrap pipeline to prevent stucking

### Security

- Error handling - changed expect/unwrap to errors
- Encodings - replaced recursive linked list with vector
- Encodings - introduced limits for p2p messages encoding
- Properly handle connection pool timeout
- Github Actions CI runs `cargo audit` (required)

## [0.9.2] - 2021-02-03

### Added

- Support for 008 protocol Edo + network support - p2p, rpc, fii

### Changed

- Migrated Tokio dependency from 0.2.x to 1.1.x
- RocksDB kv store splitted into three instances (db, context, context_actions)
- Reworked websocket implementation, now uses warp::ws instead of default ws
- Various changes around the P2P layer and bootstrapping

## [0.9.1] - 2021-01-13

### Added

- Giganode1/2 to default mainnet bootstrap peers

### Fixed

- Protocol runner restarting and IPC accept handling

## [0.9.0] - 2021-01-05

### Added

- Modification of node to be able to launch Tezos python tests in Drone CI
- Benchmarks for message encoding, ffi conversion, storage predecessor search to Drone CI
- Block applied approx. stats to log for chain_manager
- Extended statistics in merkle storage

### Changed

- Refactor shell/network channels and event/commands for actors
- Refactored chain_manager/chain_feeder + optimization to speedup bootstrap
- Optimizations of merkle storage by modifying trees in place

### Fixed

- Graceful shutdown of node and runners
- Generate invalid peer_id for identity

## [0.8.0] - 2020-11-30

### Added

- Multipass validation support for CurrentHead processing + blacklisting peers
- Support for connection to Delphinet.
- Dynamic RPC router can call Tezos's RPCs inside all protocol versions.
- Added rustfmt and clippy pipelines

### Changed

- Build is now tested on GitHub Actions instead of Travis-CI.

## [0.7.2] - 2020-11-26

### Changed

- Identity path in config for distroless docker image

## [0.7.1] - 2020-11-04

### Changed

- Logging cleanup

## [0.7.0] - 2020-10-28

### Added

- Added support for reorg + CI Drone test
- Validation for new current head after apply
- Validation for accept branch only if fitness increases
- Operation pre-validation before added to mempool

### Changed

- Skip_list was changed to merkle implementation for context

## [0.6.0] - 2020-10-20

### Added

- Added distroless docker builds
- Drone pipeline for releasing docker images (develop, master/tag)

### Fixed

- Cleanup unnecessary clones + some small optimization
- Sandbox improved error handling + cleanup


## [0.5.0] - 2020-09-30

### Added

- New OCaml FFI `ocaml-interop` integration
- Integration test for chain_manager through p2p layer

### Changed

- New library `tezos/identity` for generate/validate identity/pow in rust
- Several structs/algorithms unnecessary `clone` optimization
- Refactoring and cleanup

### Removed

- Generate identity through OCaml FFI (reimplemented in `tezos/identity`)

### Security

- Added `#![forbid(unsafe_code)]` to (almost every) modules

## [0.4.0] - 2020-09-16

### Added

- More verbose error handling in the sandbox launcher.
- New rpc `forge/operations`.
- New docker-compose file to start a setup with the sandbox launcher, tezedge-explorer front-end and tezedge-debugger.

### Fixed

- Various bugs in the sandbox launcher.


## [0.3.0] - 2020-08-31

### Added

- New configuration parameter `--disable-bootstrap-lookup` to turn off DNS lookup for peers (e.g. used for tests or sandbox).
- New configuration parameter `--db-cfg-max-threads` to better control system resources.
- New RPCs to make baking in sandbox mode possible with tezos-client.
- Support for MacOS (10.13 and newer).
- Enabling core dumps in debug mode (if not set), set max open files for process
- New sandbox module to launch the light-node via RPCs.

### Changed

- Resolved various clippy warnings/errors.
- Drone test runs offline with carthagenet-snapshoted nodes.
- New OCaml FFI - `ocaml-rs` was replaced with a new custom library based on `caml-oxide` to get GC under control and improve performance.
- P2P bootstrap process - NACK version control after metadata exchange.

## [0.2.0] - 2020-07-29

### Added

- RPCs for every protocol now support the Tezos indexer 'blockwatch/tzindex'.
- Support for connecting to Mainnet.
- Support for sandboxing, which means an empty TezEdge can be initialized with `tezos-client` for "activate protocol" and do "transfer" operation.

### Changed

- FFI upgrade based on Tezos gitlab latest-release (v7.2), now supports OCaml 4.09.1
- Support for parallel access (readonly context) to Tezos FFI OCaml runtime through r2d2 connection pooling.


## [0.1.0] - 2020-06-25

### Added

- Mempool P2P support + FFI prevalidator protocol validation.
- Support for sandboxing (used in drone tests).
- RPC for /inject/operation (draft).
- RPCs for developers for blocks and contracts.
- Possibility to run mulitple sub-process with FFI integration to OCaml.

### Changed

- Upgraded version of riker, RocksDB.
- Improved DRONE integration tests.

## [0.0.2] - 2020-06-01

### Added

- Support for connection to Carthagenet/Mainnet.
- Support for Ubuntu 20 and OpenSUSE Tumbleweed.
- RPCs for indexer blockwatch/tzindex (with drone integration test, which compares indexed data with Ocaml node against TezEdge node).
- Flags `--store-context-actions=BOOL.` If this flag is set to false, the node will persist less data to disk, which increases runtime speed.

### Changed

- P2P speed-up bootstrap - support for p2p_version 1 feature Nack_with_list, extended Nack - with potential peers to connect.

### Removed

- Storing all P2P messages (moved to tezedge-debugger), the node will persist less data to disk.

### Fixed / Security

- Remove bitvec dependency.
- Refactored FFI to Ocaml not using BigArray1's for better GC processing.

## [0.0.1] - 2020-03-31

### Added

- P2P Explorer support with dedicated RPC exposed.
- Exposed RPC for Tezos indexers.
- Ability to connect and bootstrap data from Tezos Babylonnet.
- Protocol FFI integration.

[Unreleased]: https://github.com/tezedge/tezedge/compare/v3.1.1...develop
[3.1.1]: https://github.com/tezedge/tezedge/releases/v3.1.1
[3.1.0]: https://github.com/tezedge/tezedge/releases/v3.1.0
[3.0.0]: https://github.com/tezedge/tezedge/releases/v3.0.0
[2.3.6]: https://github.com/tezedge/tezedge/releases/v2.3.6
[2.3.5]: https://github.com/tezedge/tezedge/releases/v2.3.5
[2.3.4]: https://github.com/tezedge/tezedge/releases/v2.3.4
[2.3.3]: https://github.com/tezedge/tezedge/releases/v2.3.3
[2.3.2]: https://github.com/tezedge/tezedge/releases/v2.3.2
[2.3.1]: https://github.com/tezedge/tezedge/releases/v2.3.1
[2.3.0]: https://github.com/tezedge/tezedge/releases/v2.3.0
[2.2.0]: https://github.com/tezedge/tezedge/releases/v2.2.0
[2.1.0]: https://github.com/tezedge/tezedge/releases/v2.1.0
[2.0.0]: https://github.com/tezedge/tezedge/releases/v2.0.0
[1.19.0]: https://github.com/tezedge/tezedge/releases/v1.19.0
[1.18.1]: https://github.com/tezedge/tezedge/releases/v1.18.1
[1.18.0]: https://github.com/tezedge/tezedge/releases/v1.18.0
[1.17.0]: https://github.com/tezedge/tezedge/releases/v1.17.0
[1.16.1]: https://github.com/tezedge/tezedge/releases/v1.16.1
[1.16.0]: https://github.com/tezedge/tezedge/releases/v1.16.0
[1.15.1]: https://github.com/tezedge/tezedge/releases/v1.15.1
[1.15.0]: https://github.com/tezedge/tezedge/releases/v1.15.0
[1.14.0]: https://github.com/tezedge/tezedge/releases/v1.14.0
[1.13.0]: https://github.com/tezedge/tezedge/releases/v1.13.0
[1.12.0]: https://github.com/tezedge/tezedge/releases/v1.12.0
[1.11.0]: https://github.com/tezedge/tezedge/releases/v1.11.0
[1.10.0]: https://github.com/tezedge/tezedge/releases/v1.10.0
[1.9.1]: https://github.com/tezedge/tezedge/releases/v1.9.1
[1.9.0]: https://github.com/tezedge/tezedge/releases/v1.9.0
[1.8.0]: https://github.com/tezedge/tezedge/releases/v1.8.0
[1.7.1]: https://github.com/tezedge/tezedge/releases/v1.7.1
[1.7.0]: https://github.com/tezedge/tezedge/releases/v1.7.0
[1.6.10]: https://github.com/tezedge/tezedge/releases/v1.6.10
[1.6.9]: https://github.com/tezedge/tezedge/releases/v1.6.9
[1.6.8]: https://github.com/tezedge/tezedge/releases/v1.6.8
[1.6.7]: https://github.com/tezedge/tezedge/releases/v1.6.7
[1.6.6]: https://github.com/tezedge/tezedge/releases/v1.6.6
[1.6.5]: https://github.com/tezedge/tezedge/releases/v1.6.5
[1.6.4]: https://github.com/tezedge/tezedge/releases/v1.6.4
[1.6.2]: https://github.com/tezedge/tezedge/releases/v1.6.2
[1.6.1]: https://github.com/tezedge/tezedge/releases/v1.6.1
[1.6.0]: https://github.com/tezedge/tezedge/releases/v1.6.0
[1.5.1]: https://github.com/tezedge/tezedge/releases/v1.5.1
[1.5.0]: https://github.com/tezedge/tezedge/releases/v1.5.0
[1.4.0]: https://github.com/tezedge/tezedge/releases/v1.4.0
[1.3.1]: https://github.com/tezedge/tezedge/releases/v1.3.1
[1.2.0]: https://github.com/tezedge/tezedge/releases/v1.2.0
[1.1.4]: https://github.com/tezedge/tezedge/releases/v1.1.4
[1.1.3]: https://github.com/tezedge/tezedge/releases/v1.1.3
[1.1.2]: https://github.com/tezedge/tezedge/releases/v1.1.2
[1.1.0]: https://github.com/tezedge/tezedge/releases/v1.1.0
[1.0.0]: https://github.com/tezedge/tezedge/releases/v1.0.0
[0.9.2]: https://github.com/tezedge/tezedge/releases/v0.9.2
[0.9.1]: https://github.com/tezedge/tezedge/releases/v0.9.1
[0.9.0]: https://github.com/tezedge/tezedge/releases/v0.9.0
[0.8.0]: https://github.com/tezedge/tezedge/releases/v0.8.0
[0.7.2]: https://github.com/tezedge/tezedge/releases/v0.7.2
[0.7.1]: https://github.com/tezedge/tezedge/releases/v0.7.1
[0.7.0]: https://github.com/tezedge/tezedge/releases/v0.7.0
[0.6.0]: https://github.com/tezedge/tezedge/releases/v0.6.0
[0.5.0]: https://github.com/tezedge/tezedge/releases/v0.5.0
[0.4.0]: https://github.com/tezedge/tezedge/releases/v0.4.0
[0.3.0]: https://github.com/tezedge/tezedge/releases/v0.3.0
[0.2.0]: https://github.com/tezedge/tezedge/releases/v0.2.0
[0.1.0]: https://github.com/tezedge/tezedge/releases/v0.1.0
[0.0.2]: https://github.com/tezedge/tezedge/releases/v0.0.2
[0.0.1]: https://github.com/tezedge/tezedge/releases/v0.0.1
___
The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).
