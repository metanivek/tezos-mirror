// SPDX-FileCopyrightText: 2023-2025 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024-2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

mod move_semantics;
mod pointer_apply;

use std::fs;
use std::str;

use arbitrary_int::u31;
use move_semantics::ImmutableState;
use move_semantics::MutableState;
use num_enum::IntoPrimitive;
use num_enum::TryFromPrimitive;
use ocaml::Pointer;
use ocaml::ToValue;
use pointer_apply::ApplyReadOnly;
use pointer_apply::apply_imm;
use pointer_apply::apply_mut;
use sha2::Digest;
use sha2::Sha256;

use crate::pvm::PvmHooks;
use crate::pvm::PvmStatus;
use crate::pvm::node_pvm::NodePvm;
use crate::pvm::node_pvm::PvmStorage;
use crate::pvm::node_pvm::PvmStorageError;
use crate::state_backend::ManagerReadWrite;
use crate::state_backend::hash::Hash;
use crate::state_backend::owned_backend::Owned;
use crate::state_backend::proof_backend::proof;
use crate::state_backend::proof_backend::proof::serialise_proof;
use crate::storage;
use crate::storage::StorageError;

const HERMIT_LOADER: &[u8] = include_bytes!("../../assets/hermit-loader");

type OcamlFallible<T> = Result<T, ocaml::Error>;

#[ocaml::sig]
pub struct Repo(PvmStorage);

#[ocaml::sig]
pub type State = ImmutableState<NodePvm<Owned>>;

#[ocaml::sig]
pub type MutState = MutableState<NodePvm<Owned>>;

#[ocaml::sig]
pub struct Id(storage::Hash);

ocaml::custom!(Repo);
ocaml::custom!(Id);

#[derive(ocaml::FromValue, ocaml::ToValue, IntoPrimitive, TryFromPrimitive, strum::EnumCount)]
#[ocaml::sig("Evaluating | Waiting_for_input | Waiting_for_reveal")]
#[repr(u8)]
pub enum Status {
    Evaluating,
    WaitingForInput,
    WaitingForReveal,
}

// Check that [`PvmStatus`] and [`Status`] can be coerced into each other.
const STATUS_ENUM_COERCIBLE: bool = {
    if <PvmStatus as strum::EnumCount>::COUNT != <Status as strum::EnumCount>::COUNT
        || <Status as strum::EnumCount>::COUNT != 3
    {
        panic!("Not coercible!");
    }

    if PvmStatus::Evaluating as u8 != Status::Evaluating as u8 {
        panic!("Not coercible!");
    }

    if PvmStatus::WaitingForInput as u8 != Status::WaitingForInput as u8 {
        panic!("Not coercible!");
    }

    if PvmStatus::WaitingForReveal as u8 != Status::WaitingForReveal as u8 {
        panic!("Not coercible!");
    }

    true
};

impl From<PvmStatus> for Status {
    fn from(item: PvmStatus) -> Self {
        if STATUS_ENUM_COERCIBLE {
            unsafe { std::mem::transmute::<PvmStatus, Self>(item) }
        } else {
            unreachable!()
        }
    }
}

impl From<Status> for PvmStatus {
    fn from(item: Status) -> Self {
        if STATUS_ENUM_COERCIBLE {
            unsafe { std::mem::transmute::<Status, Self>(item) }
        } else {
            unreachable!()
        }
    }
}

/// Wrapper to convert a Rust allocated byte-array to an OCaml allocated Bytes type
pub struct BytesWrapper(Box<[u8]>);

unsafe impl ocaml::ToValue for BytesWrapper {
    fn to_value(&self, _rt: &ocaml::Runtime) -> ocaml::Value {
        unsafe { ocaml::Value::bytes(&self.0) }
    }
}

/// Input values are passed into the PVM after an input request has been made.
#[derive(ocaml::FromValue)]
#[ocaml::sig("Inbox_message of int32 * int64 * bytes | Reveal of bytes")]
pub enum Input<'a> {
    InboxMessage(u32, u64, &'a [u8]),
    Reveal(&'a [u8]),
}

/// Describes possible input requests the PVM may ask for during execution.
#[derive(ocaml::ToValue)]
#[ocaml::sig("No_input_required | Initial | First_after of int32 * int64 | Needs_reveal of bytes")]
pub enum InputRequest {
    NoInputRequired,
    Initial,
    FirstAfter(u32, u64),
    NeedsReveal(BytesWrapper),
}

ocaml::custom!(InputRequest);

// This representation guarantees that on the OCaml side, when converting to `Raw_level_repr.t`
// no runtime exceptions will be thrown.
/// Wrapper to convert a Rust unsigned 31 bit value to an OCaml int32 type
pub struct RawLevel(u31);

unsafe impl ocaml::ToValue for RawLevel {
    fn to_value(&self, _rt: &ocaml::Runtime) -> ocaml::Value {
        // Make sure the underlying type is u32 to ensure a no-op conversion to OCaml values
        let wrapped_value: u32 = self.0.value();
        // Make sure the ocaml-rs conversion is for precisely converting into an OCaml i32
        unsafe { ocaml::Value::int32(wrapped_value as i32) }
    }
}

/// Metadata of an output message
#[derive(ocaml::ToValue)]
#[ocaml::sig("{message_index : int64; outbox_level : int32}")]
pub struct OutputInfo {
    pub message_index: u64,
    pub outbox_level: RawLevel,
}

ocaml::custom!(OutputInfo);

/// A value of this type is generated as part of successfully verifying an output proof.
#[derive(ocaml::ToValue)]
#[ocaml::sig("{info : output_info; encoded_message : bytes}")]
pub struct Output {
    pub info: OutputInfo,
    pub encoded_message: BytesWrapper,
}

// TODO RV-365 Implement OutputProof types
#[ocaml::sig]
pub struct OutputProof;

ocaml::custom!(OutputProof);

impl<'a> PvmHooks<'a> {
    fn from_printer(printer: ocaml::Value, gc: &'a ocaml::Runtime) -> PvmHooks<'a> {
        let putchar = move |c: u8| unsafe {
            ocaml::Value::call(&printer, gc, [ocaml::Int::from(c)])
                .expect("compute_step: putchar error")
        };
        PvmHooks::new(putchar)
    }
}

#[ocaml::func]
#[ocaml::sig("state -> mut_state")]
pub fn octez_riscv_from_imm(state: Pointer<State>) -> Pointer<MutState> {
    state.as_ref().to_mut_state().into()
}

#[ocaml::func]
#[ocaml::sig("mut_state -> state")]
pub fn octez_riscv_to_imm(state: Pointer<MutState>) -> Pointer<State> {
    state.as_ref().to_imm_state().into()
}

#[ocaml::func]
#[ocaml::sig("bytes -> id")]
pub fn octez_riscv_id_unsafe_of_raw_bytes(s: &[u8]) -> Pointer<Id> {
    assert!(s.len() == storage::DIGEST_SIZE);
    let hash: storage::Hash = s.try_into().unwrap();
    Id(hash).into()
}

#[ocaml::func]
#[ocaml::sig("id -> bytes")]
pub fn octez_riscv_storage_id_to_raw_bytes(id: Pointer<Id>) -> [u8; 32] {
    id.as_ref().0.into()
}

#[ocaml::func]
#[ocaml::sig("id -> id -> bool")]
pub fn octez_riscv_storage_id_equal(id1: Pointer<Id>, id2: Pointer<Id>) -> bool {
    id1.as_ref().0 == id2.as_ref().0
}

#[ocaml::func]
#[ocaml::sig("state -> state -> bool")]
pub fn octez_riscv_storage_state_equal(state1: Pointer<State>, state2: Pointer<State>) -> bool {
    state1.apply_ro(|pvm1| state2.apply_ro(|pvm2| pvm1 == pvm2))
}

#[ocaml::func]
#[ocaml::sig("unit -> state")]
pub fn octez_riscv_storage_state_empty() -> Pointer<State> {
    ImmutableState::new(NodePvm::empty()).into()
}

#[ocaml::func]
#[ocaml::sig("string -> repo")]
pub fn octez_riscv_storage_load(path: String) -> OcamlFallible<Pointer<Repo>> {
    match PvmStorage::load(path) {
        Ok(repo) => Ok(Repo(repo).into()),
        Err(e) => Err(ocaml::Error::Error(Box::new(e))),
    }
}

#[ocaml::func]
#[ocaml::sig("repo -> unit")]
pub fn octez_riscv_storage_close(_repo: Pointer<Repo>) {}

#[ocaml::func]
#[ocaml::sig("repo -> state -> id")]
pub fn octez_riscv_storage_commit(
    mut repo: Pointer<Repo>,
    state: Pointer<State>,
) -> OcamlFallible<Pointer<Id>> {
    state.apply_ro(|pvm| match repo.as_mut().0.commit(pvm) {
        Ok(hash) => Ok(Id(hash).into()),
        Err(e) => Err(ocaml::Error::Error(Box::new(e))),
    })
}

#[ocaml::func]
#[ocaml::sig("repo -> id -> state option")]
pub fn octez_riscv_storage_checkout(
    repo: Pointer<Repo>,
    id: Pointer<Id>,
) -> OcamlFallible<Option<Pointer<State>>> {
    let id = &id.as_ref().0;
    match repo.as_ref().0.checkout(id) {
        Ok(pvm) => Ok(Some(ImmutableState::new(pvm).into())),
        Err(PvmStorageError::StorageError(StorageError::NotFound(_))) => Ok(None),
        Err(e) => Err(ocaml::Error::Error(Box::new(e))),
    }
}

#[ocaml::func]
#[ocaml::sig("state -> status")]
pub fn octez_riscv_get_status(state: Pointer<State>) -> Status {
    state.apply_ro(NodePvm::get_status).into()
}

#[ocaml::func]
#[ocaml::sig("mut_state -> status")]
pub fn octez_riscv_mut_get_status(state: Pointer<MutState>) -> Status {
    state.apply_ro(NodePvm::get_status).into()
}

#[ocaml::func]
#[ocaml::sig("status -> string")]
pub fn octez_riscv_string_of_status(status: Status) -> String {
    PvmStatus::from(status).to_string()
}

#[ocaml::func]
#[ocaml::sig("state -> state")]
pub fn octez_riscv_compute_step(state: Pointer<State>) -> Pointer<State> {
    apply_imm(state, |pvm| pvm.compute_step(&mut PvmHooks::default())).0
}

#[ocaml::func]
#[ocaml::sig("state -> (int -> unit) -> state")]
pub fn octez_riscv_compute_step_with_debug(
    state: Pointer<State>,
    printer: ocaml::Value,
) -> Pointer<State> {
    let mut hooks = PvmHooks::from_printer(printer, gc);

    apply_imm(state, |pvm| pvm.compute_step(&mut hooks)).0
}

#[ocaml::func]
#[ocaml::sig("int64 -> state -> (state * int64)")]
pub fn octez_riscv_compute_step_many(
    max_steps: u64,
    state: Pointer<State>,
) -> (Pointer<State>, i64) {
    apply_imm(state, |pvm| {
        pvm.compute_step_many(&mut PvmHooks::default(), max_steps as usize)
    })
}

#[ocaml::func]
#[ocaml::sig("int64 -> mut_state -> int64")]
pub fn octez_riscv_mut_compute_step_many(max_steps: u64, state: Pointer<MutState>) -> i64 {
    apply_mut(state, |pvm| {
        pvm.compute_step_many(&mut PvmHooks::default(), max_steps as usize)
    })
}

#[ocaml::func]
#[ocaml::sig("int64 -> state -> (int -> unit) -> (state * int64)")]
pub fn octez_riscv_compute_step_many_with_debug(
    max_steps: u64,
    state: Pointer<State>,
    printer: ocaml::Value,
) -> (Pointer<State>, i64) {
    let mut hooks = PvmHooks::from_printer(printer, gc);

    apply_imm(state, |pvm| {
        pvm.compute_step_many(&mut hooks, max_steps as usize)
    })
}

#[ocaml::func]
#[ocaml::sig("int64 -> mut_state -> (int -> unit) -> int64")]
pub fn octez_riscv_mut_compute_step_many_with_debug(
    max_steps: u64,
    state: Pointer<MutState>,
    printer: ocaml::Value,
) -> i64 {
    let mut hooks = PvmHooks::from_printer(printer, gc);

    apply_mut(state, |pvm| {
        pvm.compute_step_many(&mut hooks, max_steps as usize)
    })
}

#[ocaml::func]
#[ocaml::sig("state -> int64")]
pub fn octez_riscv_get_tick(state: Pointer<State>) -> u64 {
    state.apply_ro(NodePvm::get_tick)
}

#[ocaml::func]
#[ocaml::sig("mut_state -> int64")]
pub fn octez_riscv_mut_get_tick(state: Pointer<MutState>) -> u64 {
    state.apply_ro(NodePvm::get_tick)
}

#[ocaml::func]
#[ocaml::sig("state -> int32 option")]
pub fn octez_riscv_get_level(state: Pointer<State>) -> Option<u32> {
    state.apply_ro(NodePvm::get_current_level)
}

#[ocaml::func]
#[ocaml::sig("mut_state -> int32 option")]
pub fn octez_riscv_mut_get_level(state: Pointer<MutState>) -> Option<u32> {
    state.apply_ro(NodePvm::get_current_level)
}

fn verify_checksum(contents: &[u8], checksum: &str) -> bool {
    let mut hasher = Sha256::new();
    hasher.update(contents);
    let digest = format!("{:x}", hasher.finalize());
    digest == checksum
}

fn read_boot_sector_binary(path: &str, checksum: &str) -> Vec<u8> {
    let binary = fs::read(path).unwrap_or_else(|_| {
        panic!("Error installing boot sector: could not read binary at {path}")
    });
    assert!(
        verify_checksum(&binary, checksum),
        "Error installing boot sector: checksum mismatch for {path}"
    );
    binary
}

#[ocaml::func]
#[ocaml::sig("state -> bytes -> state")]
pub fn octez_riscv_install_boot_sector(
    state: Pointer<State>,
    boot_sector: &[u8],
) -> Pointer<State> {
    // RISC-V kernels are too large to be originated directly. In order to
    // temporarily bypass this limitation (TODO: RV-109 Port kernel installer to RISC-V)
    // the boot sector is installed by loading it from a path passed at origination
    // after checking consistency with the provided checksum. Optionally, the Hermit loader
    // can also be passed in the origination string:
    // "kernel:<path to kernel>:<kernel checksum>[:<path to loader>:<loader checksum>]"
    // Any string not matching this format will be treated as an actual kernel to be installed.
    let install_loader_and_kernel = |pvm: &mut NodePvm, boot_sector| {
        if let Ok(s) = str::from_utf8(boot_sector) {
            let parts: Vec<&str> = s.split(':').collect();
            match parts.as_slice() {
                ["kernel", kernel_path, kernel_checksum] => {
                    let kernel = read_boot_sector_binary(kernel_path, kernel_checksum);
                    return pvm.install_boot_sector(HERMIT_LOADER, &kernel);
                }
                [
                    "kernel",
                    kernel_path,
                    kernel_checksum,
                    loader_path,
                    loader_checksum,
                ] => {
                    let kernel = read_boot_sector_binary(kernel_path, kernel_checksum);
                    let loader = read_boot_sector_binary(loader_path, loader_checksum);
                    return pvm.install_boot_sector(&loader, &kernel);
                }
                _ => (),
            }
        }
        pvm.install_boot_sector(HERMIT_LOADER, boot_sector);
    };

    apply_imm(state, |pvm| install_loader_and_kernel(pvm, boot_sector)).0
}

#[ocaml::func]
#[ocaml::sig("state -> bytes")]
pub fn octez_riscv_state_hash(state: Pointer<State>) -> [u8; 32] {
    state.apply_ro(NodePvm::hash).into()
}

#[ocaml::func]
#[ocaml::sig("mut_state -> bytes")]
pub fn octez_riscv_mut_state_hash(state: Pointer<MutState>) -> [u8; 32] {
    state.apply_ro(NodePvm::hash).into()
}

fn apply_set_input<M: ManagerReadWrite>(pvm: &mut NodePvm<M>, input: Input) {
    match input {
        Input::InboxMessage(level, message_counter, payload) => {
            pvm.set_input_message(level, message_counter, payload.to_vec())
        }
        Input::Reveal(reveal_data) => {
            pvm.set_reveal_response(reveal_data);
        }
    }
}

#[ocaml::func]
#[ocaml::sig("state -> input -> state")]
pub fn octez_riscv_set_input(state: Pointer<State>, input: Input) -> Pointer<State> {
    apply_imm(state, |pvm| apply_set_input(pvm, input)).0
}

#[ocaml::func]
#[ocaml::sig("mut_state -> input -> unit")]
pub fn octez_riscv_mut_set_input(state: Pointer<MutState>, input: Input) -> () {
    apply_mut(state, |pvm| apply_set_input(pvm, input))
}

#[ocaml::func]
#[ocaml::sig("state -> int64")]
pub fn octez_riscv_get_message_counter(state: Pointer<State>) -> u64 {
    state.apply_ro(NodePvm::get_message_counter)
}

#[ocaml::func]
#[ocaml::sig("mut_state -> int64")]
pub fn octez_riscv_mut_get_message_counter(state: Pointer<MutState>) -> u64 {
    state.apply_ro(NodePvm::get_message_counter)
}

#[ocaml::func]
#[ocaml::sig("repo -> id -> string -> (unit, [`Msg of string]) result")]
pub unsafe fn octez_riscv_storage_export_snapshot(
    repo: Pointer<Repo>,
    id: Pointer<Id>,
    path: &str,
) -> Result<(), ocaml::Value> {
    let id = &id.as_ref().0;
    repo.as_ref().0.export_snapshot(id, path).map_err(|e| {
        let s = format!("{e:?}");
        ocaml::Value::hash_variant(gc, "Msg", Some(s.to_value(gc)))
    })
}

/// Proofs
#[ocaml::sig]
pub type Proof = proof::Proof;

ocaml::custom!(Proof);

#[ocaml::func]
#[ocaml::sig("proof -> bytes")]
pub fn octez_riscv_proof_start_state(_proof: Pointer<Proof>) -> [u8; 32] {
    todo!()
}

#[ocaml::func]
#[ocaml::sig("proof -> bytes")]
pub fn octez_riscv_proof_stop_state(_proof: Pointer<Proof>) -> [u8; 32] {
    todo!()
}

#[ocaml::func]
#[ocaml::sig("input option -> proof -> input_request option")]
// Allow some warnings while this method goes through iterations.
#[allow(unreachable_code, unused_variables, clippy::diverging_sub_expression)]
pub unsafe fn octez_riscv_verify_proof(
    proof: Pointer<Proof>,
    input: Option<Input>,
) -> Option<Pointer<InputRequest>> {
    let mut state = NodePvm::from_proof(proof.as_ref().tree())?;

    match input {
        Some(input) => apply_set_input(&mut state, input),
        None => state.compute_step(&mut PvmHooks::none()),
    }

    // TODO: RV-362: Check the final state
    let hash: Hash = todo!("Can't obtain the final state hash just yet");

    if hash != proof.as_ref().final_state_hash() {
        return None;
    }

    // TODO: RV-336: Need to construct InputRequest
    Some(todo!())
}

#[ocaml::func]
#[ocaml::sig("input option -> state -> proof option")]
pub unsafe fn octez_riscv_produce_proof(
    _input: Option<Input>,
    _state: Pointer<State>,
) -> Option<Pointer<InputRequest>> {
    None
}

#[ocaml::func]
#[ocaml::sig("proof -> bytes")]
pub unsafe fn octez_riscv_serialise_proof(proof: Pointer<Proof>) -> ocaml::Value {
    // Safety: the function needs to return the same `ocaml::Value` as in the signature.
    // In this case, an ocaml bytes value has to be returned.
    let serialisation: Vec<u8> = serialise_proof(proof.as_ref()).collect();
    ocaml::Value::bytes(serialisation)
}

#[ocaml::func]
#[ocaml::sig("bytes -> (proof, string) Result.t")]
pub fn octez_riscv_deserialise_proof(_bytes: &[u8]) -> Result<Pointer<Proof>, String> {
    todo!("RV-555")
}

#[ocaml::func]
#[ocaml::sig("output_proof -> output_info")]
pub fn octez_riscv_output_info_of_output_proof(
    _output_proof: Pointer<OutputProof>,
) -> Result<Pointer<OutputInfo>, String> {
    todo!()
}

#[ocaml::func]
#[ocaml::sig("output_proof -> bytes")]
pub fn octez_riscv_state_of_output_proof(_output_proof: Pointer<OutputProof>) -> [u8; 32] {
    todo!()
}

#[ocaml::func]
#[ocaml::sig("output_proof -> output option")]
pub fn octez_riscv_verify_output_proof(
    _output_proof: Pointer<OutputProof>,
) -> Option<Pointer<Output>> {
    todo!()
}

#[ocaml::func]
#[ocaml::sig("output_proof -> bytes")]
pub fn octez_riscv_serialise_output_proof(_output_proof: Pointer<OutputProof>) -> ocaml::Value {
    // TODO RV-365 Implement Output & OutputProof types
    // Ue similar implementation to octez_riscv_serialise_proof
    todo!()
}

#[ocaml::func]
#[ocaml::sig("bytes -> (output_proof, string) result")]
pub fn octez_riscv_deserialise_output_proof(_bytes: &[u8]) -> Result<Pointer<OutputProof>, String> {
    todo!()
}

#[ocaml::func]
#[ocaml::sig("state -> bytes")]
pub fn octez_riscv_get_reveal_request(state: Pointer<State>) -> BytesWrapper {
    let serialised_reveal_request: Vec<u8> = state.apply_ro(NodePvm::get_reveal_request);
    BytesWrapper(serialised_reveal_request.into_boxed_slice())
}

#[ocaml::func]
#[ocaml::sig("mut_state -> bytes")]
pub fn octez_riscv_mut_get_reveal_request(state: Pointer<MutState>) -> BytesWrapper {
    let serialised_reveal_request: Vec<u8> = state.apply_ro(NodePvm::get_reveal_request);
    BytesWrapper(serialised_reveal_request.into_boxed_slice())
}

#[ocaml::func]
#[ocaml::sig("state -> state")]
pub fn octez_riscv_insert_failure(state: Pointer<State>) -> Pointer<State> {
    apply_imm(state, |pvm| pvm.insert_failure()).0
}

#[ocaml::func]
#[ocaml::sig("mut_state -> unit")]
pub fn octez_riscv_mut_insert_failure(state: Pointer<MutState>) {
    apply_mut(state, |pvm| pvm.insert_failure())
}
