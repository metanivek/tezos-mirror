// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Implementation of Risc-V's 32 & 64 bit C extensions for RISC-V
//!
//! Chapter 16,5 - Unprivileged spec

use crate::instruction_context::ICB;
use crate::machine_state::registers::NonZeroXRegister;

/// Copies the value in register `rs2` into register `rd_rs1`.
///
/// Relevant RISC-V opcodes:
/// - C.MV
/// - ADD
/// - SUB
/// - OR
/// - XOR
/// - SLL
/// - SRL
/// - SRA
pub fn run_mv(icb: &mut impl ICB, rd_rs1: NonZeroXRegister, rs2: NonZeroXRegister) {
    let rs2_val = icb.xregister_read_nz(rs2);
    icb.xregister_write_nz(rd_rs1, rs2_val)
}

/// Does nothing.
///
/// Relevant RISC-V opcodes:
/// - C.NOP
/// - ADDI
/// - C.ADDI4SPN
/// - C.ANDI
/// - C.SRLI
/// - C.SRAI
/// - C.AND
/// - C.OR
/// - C.XOR
/// - BNE
/// - C.BNEZ
/// - C.SUB
pub fn run_nop(_icb: &mut impl ICB) {}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::backend_test;
    use crate::create_state;
    use crate::interpreter::i::run_add;
    use crate::machine_state::MachineCoreState;
    use crate::machine_state::MachineCoreStateLayout;
    use crate::machine_state::memory::M4K;
    use crate::machine_state::registers::nz;

    backend_test!(test_add_mv, F, {
        let imm_rs1_res = [
            (0_i64, 0_u64, 0_u64),
            (0, 0xFFF0_0420, 0xFFF0_0420),
            (-1, 0, 0xFFFF_FFFF_FFFF_FFFF),
            (1_000_000, -123_000_987_i64 as u64, -122_000_987_i64 as u64),
            (1_000_000, 123_000_987, 124_000_987),
            (-1, -321_000_000_000_i64 as u64, -321_000_000_001_i64 as u64),
        ];

        for (imm, rs1, res) in imm_rs1_res {
            let mut state = create_state!(MachineCoreState, MachineCoreStateLayout<M4K>, F, M4K);

            state.hart.xregisters.write_nz(nz::a3, rs1);
            state.hart.xregisters.write_nz(nz::a4, imm as u64);

            run_add(&mut state, nz::a3, nz::a4, nz::a3);
            assert_eq!(state.hart.xregisters.read_nz(nz::a3), res);
            run_mv(&mut state, nz::a4, nz::a3);
            assert_eq!(state.hart.xregisters.read_nz(nz::a4), res);
        }
    });
}
