// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use super::{Args, Instruction, OpCode};
use crate::{
    default::ConstDefault,
    machine_state::registers::{nz, NonZeroXRegister},
    parser::{
        instruction::{
            CIBTypeArgs, CRTypeArgs, InstrWidth, NonZeroRdITypeArgs, NonZeroRdRTypeArgs,
            SplitITypeArgs,
        },
        split_x0, XRegisterParsed,
    },
};

impl Instruction {
    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for the [`OpCode::Add`].
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

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::Mv`].
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

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::Li`].
    pub(crate) fn new_li(rd: NonZeroXRegister, imm: i64, width: InstrWidth) -> Self {
        Self {
            opcode: OpCode::Li,
            args: Args {
                rd: rd.into(),
                // We are adding default values for rs1 and rs2 as NonZeroXRegister::x1
                // to be explicit that it is of NonZeroXRegister type.
                rs1: NonZeroXRegister::x1.into(),
                rs2: NonZeroXRegister::x1.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for  [`OpCode::Nop`].
    pub(crate) fn new_nop(width: InstrWidth) -> Self {
        Self {
            opcode: OpCode::Nop,
            args: Args {
                // We are adding default values for rd, rs1 and rs2 as NonZeroXRegister::x1
                // to be explicit that they are of NonZeroXRegister type.
                rd: NonZeroXRegister::x1.into(),
                rs1: NonZeroXRegister::x1.into(),
                rs2: NonZeroXRegister::x1.into(),
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::Addi`].
    pub(crate) fn new_addi(
        rd: NonZeroXRegister,
        rs1: NonZeroXRegister,
        imm: i64,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::Addi,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                // We are adding a default value for rs2 as NonZeroXRegister::x1
                // to be explicit that it is of NonZeroXRegister type.
                rs2: NonZeroXRegister::x1.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::Andi`].
    pub(crate) fn new_andi(
        rd: NonZeroXRegister,
        rs1: NonZeroXRegister,
        imm: i64,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::Andi,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                // We are adding a default value for rs2 as NonZeroXRegister::x1
                // to be explicit that it is of NonZeroXRegister type.
                rs2: NonZeroXRegister::x1.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::Ori`].
    pub(crate) fn new_ori(
        rd: NonZeroXRegister,
        rs1: NonZeroXRegister,
        imm: i64,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::Ori,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                // We are adding a default value for rs2 as NonZeroXRegister::x1
                // to be explicit that it is of NonZeroXRegister type.
                rs2: NonZeroXRegister::x1.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::Xori`].
    pub(crate) fn new_xori(
        rd: NonZeroXRegister,
        rs1: NonZeroXRegister,
        imm: i64,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::Xori,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                // We are adding a default value for rs2 as NonZeroXRegister::x1
                // to be explicit that it is of NonZeroXRegister type.
                rs2: NonZeroXRegister::x1.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::Slli`].
    pub(crate) fn new_slli(
        rd: NonZeroXRegister,
        rs1: NonZeroXRegister,
        imm: i64,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::Slli,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                // We are adding a default value for rs2 as NonZeroXRegister::x1
                // to be explicit that it is of NonZeroXRegister type.
                rs2: NonZeroXRegister::x1.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::Srli`].
    pub(crate) fn new_srli(
        rd: NonZeroXRegister,
        rs1: NonZeroXRegister,
        imm: i64,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::Srli,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                // We are adding a default value for rs2 as NonZeroXRegister::x1
                // to be explicit that it is of NonZeroXRegister type.
                rs2: NonZeroXRegister::x1.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::Srai`].
    pub(crate) fn new_srai(
        rd: NonZeroXRegister,
        rs1: NonZeroXRegister,
        imm: i64,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::Srai,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                // We are adding a default value for rs2 as NonZeroXRegister::x1
                // to be explicit that it is of NonZeroXRegister type.
                rs2: NonZeroXRegister::x1.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::And`].
    pub(crate) fn new_and(
        rd: NonZeroXRegister,
        rs1: NonZeroXRegister,
        rs2: NonZeroXRegister,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::And,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                rs2: rs2.into(),
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::Or`].
    pub(crate) fn new_or(
        rd: NonZeroXRegister,
        rs1: NonZeroXRegister,
        rs2: NonZeroXRegister,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::Or,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                rs2: rs2.into(),
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::Xor`].
    pub(crate) fn new_xor(
        rd: NonZeroXRegister,
        rs1: NonZeroXRegister,
        rs2: NonZeroXRegister,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::Xor,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                rs2: rs2.into(),
                width,
                ..Args::DEFAULT
            },
        }
    }
}

impl Instruction {
    /// Convert [`InstrCacheable::Add`] according to whether registers are non-zero.
    ///
    /// [`InstrCacheable::Add`]: crate::parser::instruction::InstrCacheable::Add
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

    /// Convert [`InstrCacheable::Addi`] according to whether registers are non-zero.
    ///
    /// [`InstrCacheable::Addi`]: crate::parser::instruction::InstrCacheable::Addi
    pub(super) fn from_ic_addi(args: &SplitITypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match (args.rd, args.rs1) {
            (X::X0, _) => Instruction::new_nop(InstrWidth::Uncompressed),
            (X::NonZero(rd), X::X0) => Instruction::new_li(rd, args.imm, InstrWidth::Uncompressed),
            (X::NonZero(rd), X::NonZero(rs1)) => {
                Instruction::new_addi(rd, rs1, args.imm, InstrWidth::Uncompressed)
            }
        }
    }

    /// Convert [`InstrCacheable::CAddi4spn`] according to whether register is non-zero.
    ///
    /// [`InstrCacheable::CAddi4spn`]: crate::parser::instruction::InstrCacheable::CAddi4spn
    pub(super) fn from_ic_caddi4spn(args: &CIBTypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match split_x0(args.rd_rs1) {
            X::X0 => Instruction::new_nop(InstrWidth::Compressed),
            X::NonZero(rd_rs1) => {
                Instruction::new_addi(rd_rs1, nz::sp, args.imm, InstrWidth::Compressed)
            }
        }
    }

    /// Convert [`InstrCacheable::Andi`] according to whether register is non-zero.
    ///
    /// [`InstrCacheable::Andi`]: crate::parser::instruction::InstrCacheable::Andi
    pub(super) fn from_ic_andi(args: &NonZeroRdITypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match split_x0(args.rs1) {
            // Bitwise AND with zero is zero: `x & 0 == 0`
            X::X0 => Instruction::new_li(args.rd, 0, InstrWidth::Uncompressed),
            X::NonZero(rs1) => {
                Instruction::new_andi(args.rd, rs1, args.imm, InstrWidth::Uncompressed)
            }
        }
    }

    /// Convert [`InstrCacheable::CAndi`] according to whether register is non-zero.
    ///
    /// [`InstrCacheable::CAndi`]: crate::parser::instruction::InstrCacheable::CAndi
    pub(super) fn from_ic_candi(args: &CIBTypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match split_x0(args.rd_rs1) {
            X::X0 => Instruction::new_nop(InstrWidth::Compressed),
            X::NonZero(rd_rs1) => {
                Instruction::new_andi(rd_rs1, rd_rs1, args.imm, InstrWidth::Compressed)
            }
        }
    }

    /// Convert [`InstrCacheable::Ori`] according to whether register is non-zero.
    ///
    /// [`InstrCacheable::Ori`]: crate::parser::instruction::InstrCacheable::Ori
    pub(super) fn from_ic_ori(args: &NonZeroRdITypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match split_x0(args.rs1) {
            // Bitwise OR with zero is identity: `x | 0 == x`
            X::X0 => Instruction::new_li(args.rd, args.imm, InstrWidth::Uncompressed),
            X::NonZero(rs1) => {
                Instruction::new_ori(args.rd, rs1, args.imm, InstrWidth::Uncompressed)
            }
        }
    }

    /// Convert [`InstrCacheable::Xori`] according to whether register is non-zero.
    ///
    /// [`InstrCacheable::Xori`]: crate::parser::instruction::InstrCacheable::Xori
    pub(super) fn from_ic_xori(args: &NonZeroRdITypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match split_x0(args.rs1) {
            // Bitwise XOR with zero is identity: `x ^ 0 == x`
            X::X0 => Instruction::new_li(args.rd, args.imm, InstrWidth::Uncompressed),
            X::NonZero(rs1) => {
                Instruction::new_xori(args.rd, rs1, args.imm, InstrWidth::Uncompressed)
            }
        }
    }

    /// Convert [`InstrCacheable::Slli`] according to whether register is non-zero.
    ///
    /// [`InstrCacheable::Slli`]: crate::parser::instruction::InstrCacheable::Slli
    pub(super) fn from_ic_slli(args: &NonZeroRdITypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match split_x0(args.rs1) {
            // Shifting 0 by any amount is 0.
            X::X0 => Instruction::new_li(args.rd, 0, InstrWidth::Uncompressed),
            X::NonZero(rs1) => {
                Instruction::new_slli(args.rd, rs1, args.imm, InstrWidth::Uncompressed)
            }
        }
    }

    /// Convert [`InstrCacheable::Srli`] according to whether registers are non-zero.
    ///
    /// [`InstrCacheable::Srli`]: crate::parser::instruction::InstrCacheable::Srli
    pub(super) fn from_ic_srli(args: &NonZeroRdITypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match split_x0(args.rs1) {
            // shifting 0 by any amount is 0.
            X::X0 => Instruction::new_li(args.rd, 0, InstrWidth::Uncompressed),
            X::NonZero(rs1) => {
                Instruction::new_srli(args.rd, rs1, args.imm, InstrWidth::Uncompressed)
            }
        }
    }

    /// Convert [`InstrCacheable::CSrli`] according to whether register is non-zero.
    ///
    /// [`InstrCacheable::CSrli`]: crate::parser::instruction::InstrCacheable::CSrli
    pub(super) fn from_ic_csrli(args: &CIBTypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match split_x0(args.rd_rs1) {
            X::X0 => Instruction::new_nop(InstrWidth::Compressed),
            X::NonZero(rd_rs1) => {
                Instruction::new_srli(rd_rs1, rd_rs1, args.imm, InstrWidth::Compressed)
            }
        }
    }

    /// Convert [`InstrCacheable::Srai`] according to whether registers are non-zero.
    ///
    /// [`InstrCacheable::Srai`]: crate::parser::instruction::InstrCacheable::Srai
    pub(super) fn from_ic_srai(args: &NonZeroRdITypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match split_x0(args.rs1) {
            // shifting 0 by any amount is 0.
            X::X0 => Instruction::new_li(args.rd, 0, InstrWidth::Uncompressed),
            X::NonZero(rs1) => {
                Instruction::new_srai(args.rd, rs1, args.imm, InstrWidth::Uncompressed)
            }
        }
    }

    /// Convert [`InstrCacheable::CSrai`] according to whether registers are non-zero.
    ///
    /// [`InstrCacheable::CSrai`]: crate::parser::instruction::InstrCacheable::CSrai
    pub(super) fn from_ic_csrai(args: &CIBTypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match split_x0(args.rd_rs1) {
            X::X0 => Instruction::new_nop(InstrWidth::Compressed),
            X::NonZero(rd_rs1) => {
                Instruction::new_srai(rd_rs1, rd_rs1, args.imm, InstrWidth::Compressed)
            }
        }
    }

    /// Convert [`InstrCacheable::And`] according to whether register is non-zero.
    ///
    /// [`InstrCacheable::And`]: crate::parser::instruction::InstrCacheable::And
    pub(super) fn from_ic_and(args: &NonZeroRdRTypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match (split_x0(args.rs1), split_x0(args.rs2)) {
            // Bitwise AND with zero is zero: `x & 0 == 0`
            (X::X0, _) | (_, X::X0) => Instruction::new_li(args.rd, 0, InstrWidth::Uncompressed),
            (X::NonZero(rs1), X::NonZero(rs2)) => {
                Instruction::new_and(args.rd, rs1, rs2, InstrWidth::Uncompressed)
            }
        }
    }

    /// Convert [`InstrCacheable::CAnd`] according to whether register is non-zero.
    ///
    /// [`InstrCacheable::CAnd`]: crate::parser::instruction::InstrCacheable::CAnd
    pub(super) fn from_ic_cand(args: &CRTypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match (split_x0(args.rd_rs1), split_x0(args.rs2)) {
            (X::X0, _) => Instruction::new_nop(InstrWidth::Compressed),
            // Bitwise AND with zero is zero: `x & 0 == 0`
            (X::NonZero(rd_rs1), X::X0) => Instruction::new_li(rd_rs1, 0, InstrWidth::Compressed),
            (X::NonZero(rd_rs1), X::NonZero(rs2)) => {
                Instruction::new_and(rd_rs1, rd_rs1, rs2, InstrWidth::Compressed)
            }
        }
    }

    /// Convert [`InstrCacheable::Or`] according to whether register is non-zero.
    ///
    /// [`InstrCacheable::Or`]: crate::parser::instruction::InstrCacheable::Or
    pub(super) fn from_ic_or(args: &NonZeroRdRTypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match (split_x0(args.rs1), split_x0(args.rs2)) {
            (X::X0, X::X0) => Instruction::new_li(args.rd, 0, InstrWidth::Uncompressed),
            (X::NonZero(rs1), X::X0) | (X::X0, X::NonZero(rs1)) => {
                Instruction::new_mv(args.rd, rs1, InstrWidth::Uncompressed)
            }
            (X::NonZero(rs1), X::NonZero(rs2)) => {
                Instruction::new_or(args.rd, rs1, rs2, InstrWidth::Uncompressed)
            }
        }
    }

    /// Convert [`InstrCacheable::COr`] according to whether registers are non-zero.
    ///
    /// [`InstrCacheable::COr`]: crate::parser::instruction::InstrCacheable::COr
    pub(super) fn from_ic_cor(args: &CRTypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match (split_x0(args.rd_rs1), split_x0(args.rs2)) {
            // if rd is 0, then the instruction is a NOP.
            // if rs2 is 0, it is the same as moving rs1 to rd, which are the same register.
            (X::X0, _) | (_, X::X0) => Instruction::new_nop(InstrWidth::Compressed),
            (X::NonZero(rd_rs1), X::NonZero(rs2)) => {
                Instruction::new_or(rd_rs1, rd_rs1, rs2, InstrWidth::Compressed)
            }
        }
    }

    /// Convert [`InstrCacheable::Xor`] according to whether register is non-zero.
    ///
    /// [`InstrCacheable::Xor`]: crate::parser::instruction::InstrCacheable::Xor
    pub(super) fn from_ic_xor(args: &NonZeroRdRTypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match (split_x0(args.rs1), split_x0(args.rs2)) {
            (X::X0, X::X0) => Instruction::new_li(args.rd, 0, InstrWidth::Uncompressed),
            (X::NonZero(rs1), X::X0) | (X::X0, X::NonZero(rs1)) => {
                Instruction::new_mv(args.rd, rs1, InstrWidth::Uncompressed)
            }
            (X::NonZero(rs1), X::NonZero(rs2)) => {
                Instruction::new_xor(args.rd, rs1, rs2, InstrWidth::Uncompressed)
            }
        }
    }

    /// Convert [`InstrCacheable::CXor`] according to whether register is non-zero.
    ///
    /// [`InstrCacheable::CXor`]: crate::parser::instruction::InstrCacheable::CXor
    pub(super) fn from_ic_cxor(args: &CRTypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match (split_x0(args.rd_rs1), split_x0(args.rs2)) {
            // if rd is 0, then the instruction is a NOP.
            // if rs2 is 0, it is the same as moving rs1 to rd, which are the same register.
            (X::X0, _) | (_, X::X0) => Instruction::new_nop(InstrWidth::Compressed),
            (X::NonZero(rd_rs1), X::NonZero(rs2)) => {
                Instruction::new_xor(rd_rs1, rd_rs1, rs2, InstrWidth::Compressed)
            }
        }
    }
}
