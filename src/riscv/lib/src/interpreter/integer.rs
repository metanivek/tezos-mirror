// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Implementation of internal opcodes appropriate for JIT compilation.

use crate::instruction_context::ICB;
use crate::machine_state::registers::NonZeroXRegister;
use crate::machine_state::registers::XRegisters;
use crate::state_backend as backend;

impl<M> XRegisters<M>
where
    M: backend::ManagerReadWrite,
{
    /// Moves the two's complement representation of `rs1` into `rd`.
    ///
    /// Relevant RISC-V opcodes:
    /// - SUB
    pub fn run_neg(&mut self, rd_rs1: NonZeroXRegister, rs2: NonZeroXRegister) {
        let result = 0_u64.wrapping_sub(self.read_nz(rs2));
        self.write_nz(rd_rs1, result)
    }
}

/// Saves in `rd` the bitwise AND between the value in `rs1` and `rs2`
///
/// Relevant RISC-V opcodes:
/// - `AND`
/// - `C.AND`
pub fn run_and(
    icb: &mut impl ICB,
    rs1: NonZeroXRegister,
    rs2: NonZeroXRegister,
    rd: NonZeroXRegister,
) {
    let lhs = icb.xregister_read_nz(rs1);
    let rhs = icb.xregister_read_nz(rs2);

    let res = icb.xvalue_bitwise_and(lhs, rhs);
    icb.xregister_write_nz(rd, res);
}

/// Saves in `rd` the bitwise OR between the value in `rs1` and `rs2`
///
/// Relevant RISC-V opcodes:
/// - `OR`
/// - `C.OR`
pub fn run_or(
    icb: &mut impl ICB,
    rs1: NonZeroXRegister,
    rs2: NonZeroXRegister,
    rd: NonZeroXRegister,
) {
    let lhs = icb.xregister_read_nz(rs1);
    let rhs = icb.xregister_read_nz(rs2);

    let res = icb.xvalue_bitwise_or(lhs, rhs);
    icb.xregister_write_nz(rd, res);
}

#[cfg(test)]
mod tests {
    use crate::backend_test;
    use crate::create_state;
    use crate::machine_state::MachineCoreState;
    use crate::machine_state::MachineCoreStateLayout;
    use crate::machine_state::memory::M4K;
    use crate::machine_state::registers::nz;
    use crate::machine_state::registers::nz::a0;

    backend_test!(test_negate, F, {
        let rs2val_rd_res = [
            (0_u64, nz::t3, 0_u64),
            (0xFFFF_FFFF_FFF0_0420, nz::t2, 0x0000_0000_000F_FBE0),
            (0xFFFF_FFFF_FFFF_FFFF, nz::t4, 0x0000_0000_0000_0001),
        ];

        for (rs2, rd, res) in rs2val_rd_res {
            let mut state = create_state!(MachineCoreState, MachineCoreStateLayout<M4K>, F, M4K);

            state.hart.xregisters.write_nz(a0, rs2);
            state.hart.xregisters.run_neg(rd, nz::a0);
            assert_eq!(state.hart.xregisters.read_nz(rd), res);
        }
    });
}
