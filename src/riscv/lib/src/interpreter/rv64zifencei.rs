// SPDX-FileCopyrightText: 2024-2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Implementation of Zifencei extension for RISC-V

use crate::machine_state::AccessType;
use crate::machine_state::CacheLayouts;
use crate::machine_state::MachineState;
use crate::machine_state::block_cache::bcall::Block;
use crate::machine_state::memory::MemoryConfig;
use crate::state_backend;

impl<MC, CL, B, M> MachineState<MC, CL, B, M>
where
    MC: MemoryConfig,
    CL: CacheLayouts,
    B: Block<MC, M>,
    M: state_backend::ManagerReadWrite,
{
    /// Execute a `fence.i` instruction.
    #[inline(always)]
    pub fn run_fencei(&mut self) {
        self.core
            .translation_cache
            .invalidate([AccessType::Instruction]);
        self.block_cache.invalidate();
    }
}
