// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Builder for turning [instructions] into functions.
//!
//! [instructions]: crate::machine_state::instruction::Instruction

use cranelift::codegen::ir::InstBuilder;
use cranelift::codegen::ir::MemFlags;
use cranelift::codegen::ir::Type;
use cranelift::codegen::ir::Value;
use cranelift::codegen::ir::condcodes::IntCC;
use cranelift::codegen::ir::types::I64;
use cranelift::frontend::FunctionBuilder;

use super::state_access::JitStateAccess;
use super::state_access::JsaCalls;
use crate::instruction_context::ICB;
use crate::instruction_context::Predicate;
use crate::machine_state::memory::Address;
use crate::machine_state::memory::MemoryConfig;
use crate::machine_state::registers::NonZeroXRegister;

/// Builder context used when lowering individual instructions within a block.
pub(super) struct Builder<'a, MC: MemoryConfig, JSA: JitStateAccess> {
    /// Cranelift function builder
    pub builder: FunctionBuilder<'a>,

    /// Helpers for calling locally imported [JitStateAccess] methods.
    pub jsa_call: JsaCalls<'a, MC, JSA>,

    /// The IR-type of pointers on the current native platform
    pub ptr: Type,

    /// Value representing a pointer to `MachineCoreState<MC, JSA>`
    pub core_ptr_val: Value,

    /// Value representing a pointer to `steps: usize`
    pub steps_ptr_val: Value,

    /// The number of steps taken within the function
    pub steps: usize,

    /// Value representing the initial value of `instr_pc`
    pub pc_val: Value,

    /// The static offset so far of the `instr_pc`
    pub pc_offset: Address,
}

impl<'a, MC: MemoryConfig, JSA: JitStateAccess> Builder<'a, MC, JSA> {
    /// Consume the builder, allowing for the function under construction to be [`finalised`].
    ///
    /// [`finalised`]: super::JIT::finalise
    pub(super) fn end(mut self) {
        // flush steps
        let steps = self
            .builder
            .ins()
            .load(self.ptr, MemFlags::trusted(), self.steps_ptr_val, 0);
        let steps = self.builder.ins().iadd_imm(steps, self.steps as i64);
        self.builder
            .ins()
            .store(MemFlags::trusted(), steps, self.steps_ptr_val, 0);

        // flush pc
        let pc_val = self
            .builder
            .ins()
            .iadd_imm(self.pc_val, self.pc_offset as i64);
        self.jsa_call
            .pc_write(&mut self.builder, self.core_ptr_val, pc_val);

        self.builder.ins().return_(&[]);
        self.builder.finalize();
    }

    /// Clear the builder context on failure.
    pub(super) fn fail(mut self) {
        // On failure, the context must be cleared to ensure a clean context for the next block to
        // be compiled.

        // Before clearing the context, we need to ensure that
        // the block compiled so far matches the ABI of the function
        //
        // In this case, we must ensure that we explicitly declare
        // a lack of return values.
        self.builder.ins().return_(&[]);

        // Clearing the context is done via `finalize`, which internally clears the
        // buffers to allow re-use.
        self.builder.finalize();
    }
}

impl<'a, MC: MemoryConfig, JSA: JitStateAccess> ICB for Builder<'a, MC, JSA> {
    type XValue = Value;
    type IResult<Value> = Value;

    /// An `I8` width value.
    type Bool = Value;

    fn xregister_read_nz(&mut self, reg: NonZeroXRegister) -> Self::XValue {
        self.jsa_call
            .xreg_read(&mut self.builder, self.core_ptr_val, reg)
    }

    fn xregister_write_nz(&mut self, reg: NonZeroXRegister, value: Self::XValue) {
        self.jsa_call
            .xreg_write(&mut self.builder, self.core_ptr_val, reg, value)
    }

    fn xvalue_of_imm(&mut self, imm: i64) -> Self::XValue {
        self.builder.ins().iconst(I64, imm)
    }

    fn xvalue_from_bool(&mut self, value: Self::Bool) -> Self::XValue {
        // unsigned extension works as boolean can never be negative (only 0 or 1)
        self.builder.ins().uextend(I64, value)
    }

    fn xvalue_negate(&mut self, value: Self::XValue) -> Self::XValue {
        self.builder.ins().ineg(value)
    }

    fn xvalue_wrapping_add(&mut self, lhs: Self::XValue, rhs: Self::XValue) -> Self::XValue {
        // wrapping add; does not depend on whether operands are signed
        self.builder.ins().iadd(lhs, rhs)
    }

    fn xvalue_wrapping_sub(&mut self, lhs: Self::XValue, rhs: Self::XValue) -> Self::XValue {
        // wrapping sub; does not depend on whether operands are signed
        self.builder.ins().isub(lhs, rhs)
    }

    fn xvalue_bitwise_and(&mut self, lhs: Self::XValue, rhs: Self::XValue) -> Self::XValue {
        self.builder.ins().band(lhs, rhs)
    }

    fn xvalue_bitwise_or(&mut self, lhs: Self::XValue, rhs: Self::XValue) -> Self::XValue {
        self.builder.ins().bor(lhs, rhs)
    }

    /// Read the effective current program counter by adding `self.pc_offset` (due to instructions
    /// already lowered into this block) to `self.pc_val` (the initial value of the program counter
    /// for the block).
    fn pc_read(&mut self) -> Self::XValue {
        self.builder
            .ins()
            .iadd_imm(self.pc_val, self.pc_offset as i64)
    }

    fn xvalue_compare(
        &mut self,
        comparison: crate::instruction_context::Predicate,
        lhs: Self::XValue,
        rhs: Self::XValue,
    ) -> Self::Bool {
        // icmp returns 1 if the condition holds, 0 if it does not.
        //
        // This matches the required semantics of bool - namely that it coerces to XValue with
        // - true => 1
        // - false => 0
        //
        // See
        // <https://docs.rs/cranelift-codegen/0.117.2/cranelift_codegen/ir/trait.InstBuilder.html#method.icmp>
        self.builder.ins().icmp(comparison, lhs, rhs)
    }

    fn ok<Value>(&mut self, val: Value) -> Self::IResult<Value> {
        val
    }

    fn map<Value, Next, F>(_res: Self::IResult<Value>, _f: F) -> Self::IResult<Next>
    where
        F: FnOnce(Value) -> Next,
    {
        todo!("RV-415: support fallible pathways in JIT")
    }

    fn and_then<Value, Next, F>(_res: Self::IResult<Value>, _f: F) -> Self::IResult<Next>
    where
        F: FnOnce(Value) -> Self::IResult<Next>,
    {
        todo!("RV-415: support fallible pathways in JIT")
    }
}

impl From<Predicate> for IntCC {
    fn from(value: Predicate) -> Self {
        match value {
            Predicate::LessThanSigned => IntCC::SignedLessThan,
            Predicate::LessThanUnsigned => IntCC::UnsignedLessThan,
        }
    }
}
