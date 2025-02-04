// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use super::{Args, Instruction, OpCode};
use crate::{
    default::ConstDefault,
    machine_state::registers::NonZeroXRegister,
    parser::{
        instruction::{InstrWidth, NonZeroRdRTypeArgs},
        split_x0, XRegisterParsed,
    },
};

impl Instruction {
    /// Create a new [`Instruction`] with the appropriate [`ArgsShape`] for the `Add` [`OpCode`].
    pub(crate) fn new_add(
        rd: NonZeroXRegister,
        rs1: NonZeroXRegister,
        rs2: NonZeroXRegister,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::Add,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                rs2: rs2.into(),
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`ArgsShape`] for the `Mv` [`OpCode`].
    pub(crate) fn new_mv(rd: NonZeroXRegister, rs2: NonZeroXRegister, width: InstrWidth) -> Self {
        Self {
            opcode: OpCode::Mv,
            args: Args {
                rd: rd.into(),
                // We are adding a default value for rs1 as NonZeroXRegister::x1
                // to be explicit that it is of NonZeroXRegister type.
                rs1: NonZeroXRegister::x1.into(),
                rs2: rs2.into(),
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`ArgsShape`] for the `Li` [`OpCode`].
    pub(crate) fn new_li(rd: NonZeroXRegister, imm: i64, width: InstrWidth) -> Self {
        Self {
            opcode: OpCode::Li,
            args: Args {
                rd: rd.into(),
                // We are adding a default values for rs1 and rs2 as NonZeroXRegister::x1
                // to be explicit that it is of NonZeroXRegister type.
                rs1: NonZeroXRegister::x1.into(),
                rs2: NonZeroXRegister::x1.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`ArgsShape`] for the `Nop` [`OpCode`].
    pub(crate) fn new_nop(width: InstrWidth) -> Self {
        Self {
            opcode: OpCode::Nop,
            args: Args {
                // We are adding a default values for rd, rs1 and rs2 as NonZeroXRegister::x1
                // to be explicit that they are of NonZeroXRegister type.
                rd: NonZeroXRegister::x1.into(),
                rs1: NonZeroXRegister::x1.into(),
                rs2: NonZeroXRegister::x1.into(),
                width,
                ..Args::DEFAULT
            },
        }
    }
}

impl Instruction {
    /// Convert [`InstrCacheable::Add`] according to whether registers are non-zero.
    pub(super) fn from_ic_add(args: &NonZeroRdRTypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match (split_x0(args.rs1), split_x0(args.rs2)) {
            (X::X0, X::X0) => Instruction::new_li(args.rd, 0, InstrWidth::Uncompressed),
            (X::NonZero(rs1), X::X0) | (X::X0, X::NonZero(rs1)) => {
                Instruction::new_mv(args.rd, rs1, InstrWidth::Uncompressed)
            }
            (X::NonZero(rs1), X::NonZero(rs2)) => {
                Instruction::new_add(args.rd, rs1, rs2, InstrWidth::Uncompressed)
            }
        }
    }
}
