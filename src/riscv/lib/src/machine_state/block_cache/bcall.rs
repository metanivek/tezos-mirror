// SPDX-FileCopyrightText: 2024-2025 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Switching of execution strategy for blocks.
//!
//! Currently just for interperation only, but will expand to cover JIT.

use super::CACHE_INSTR;
use super::ICallPlaced;
use super::run_instr;
use crate::default::ConstDefault;
use crate::jit::JCall;
use crate::jit::JIT;
use crate::jit::state_access::JitStateAccess;
use crate::machine_state::MachineCoreState;
use crate::machine_state::ProgramCounterUpdate;
use crate::machine_state::instruction::Instruction;
use crate::machine_state::memory::Address;
use crate::machine_state::memory::MemoryConfig;
use crate::state_backend::AllocatedOf;
use crate::state_backend::Atom;
use crate::state_backend::Cell;
use crate::state_backend::EnrichedCell;
use crate::state_backend::FnManager;
use crate::state_backend::ManagerBase;
use crate::state_backend::ManagerClone;
use crate::state_backend::ManagerRead;
use crate::state_backend::ManagerReadWrite;
use crate::state_backend::ManagerWrite;
use crate::state_backend::Ref;
use crate::state_backend::hash::Hash;
use crate::traps::EnvironException;
use crate::traps::Exception;

/// A block derived from a sequence of [`Instruction`] that can be directly run
/// over the [`MachineCoreState`].
///
/// This allows static dispatch of this block, via different strategies. Namely:
/// interpretation and Just-In-Time compilation.
pub trait BCall<MC: MemoryConfig, M: ManagerBase> {
    /// The number of instructions contained in the block.
    ///
    /// Executing a block will consume up to `num_instr` steps.
    fn num_instr(&self) -> usize
    where
        M: ManagerRead;

    /// Run a block against the machine state.
    ///
    /// When calling this function, there must be no partial block in progress. To ensure
    /// this, you must always run [`BlockCache::complete_current_block`] prior to fetching
    /// and running a new block.
    ///
    /// There _must_ also be sufficient steps remaining, to execute the block in full.
    ///
    /// [`BlockCache::complete_current_block`]: super::BlockCache::complete_current_block
    fn run_block(
        &self,
        core: &mut MachineCoreState<MC, M>,
        instr_pc: Address,
        steps: &mut usize,
    ) -> Result<(), EnvironException>
    where
        M: ManagerReadWrite;
}

/// State Layout for Blocks
pub type BlockLayout = (Atom<u8>, [Atom<Instruction>; CACHE_INSTR]);

/// Functionality required to construct & execute blocks.
///
/// A block is a sequence of at least one instruction, which may be executed sequentially.
/// Blocks will never contain more than [`CACHE_INSTR`] instructions.
pub trait Block<MC: MemoryConfig, M: ManagerBase> {
    /// Block construction may require additional state not kept in storage,
    /// this is then passed as a parameter to [`Block::callable`].
    type BlockBuilder: Default;

    /// Bind the block to the given allocated state.
    fn bind(allocated: AllocatedOf<BlockLayout, M>) -> Self
    where
        M::ManagerRoot: ManagerReadWrite;

    /// Given a manager morphism `f : &M -> N`, return the layout's allocated structure containing
    /// the constituents of `N` that were produced from the constituents of `&M`.
    fn struct_ref<'a, F: FnManager<Ref<'a, M>>>(&'a self) -> AllocatedOf<BlockLayout, F::Output>;

    /// Ready a block for construction.
    ///
    /// Previous instructions are removed.
    fn start_block(&mut self)
    where
        M: ManagerWrite;

    /// Push an instruction to the block.
    fn push_instr(&mut self, instr: Instruction)
    where
        M: ManagerReadWrite;

    fn num_instr(&self) -> usize
    where
        M: ManagerRead;

    /// Invalidate a block, it will no longer be callable.
    fn invalidate(&mut self)
    where
        M: ManagerWrite;

    /// Reset a block to the default state, it will no longer be callable.
    fn reset(&mut self)
    where
        M: ManagerReadWrite;

    /// Returns the underlying slice of instructions stored in the block.
    fn instr(&self) -> &[EnrichedCell<ICallPlaced<MC, M>, M>]
    where
        M: ManagerRead;

    /// Get a callable block from an entry. The entry must have passed the address and fence
    /// checks.
    ///
    /// # Safety
    ///
    /// The `block_builder` must be the same as the block builder given to the `compile` call that
    /// (may) have natively compiled this block to machine code.
    ///
    /// This ensures that the builder in question is guaranteed to be alive, for at least as long
    /// as this block may be run via `BCall::run_block`.
    unsafe fn callable<'a>(
        &mut self,
        block_builder: &'a mut Self::BlockBuilder,
    ) -> Option<&mut (impl BCall<MC, M> + ?Sized + 'a)>
    where
        M: ManagerRead + 'a;

    /// Returns the block hash of instructions
    fn block_hash(&self) -> &BlockHash;
}

/// The hash of a block is by default `Dirty` - ie it may be under construction.
///
/// Only once blocks are made callable, within the specific context of the current backend, is
/// the hash calculated. At this point the block is declared `Runnable`.
#[derive(Debug, PartialEq, Eq)]
pub enum BlockHash {
    /// This block may be under construction.
    ///
    /// In order for any such block to run, it may be made runnable. First by calculating
    /// its block hash and triggering any side effects (such as JIT compilation).
    Dirty,
    /// This block can be run.
    Runnable(Hash),
}

impl BlockHash {
    fn is_dirty(&self) -> bool {
        self == &Self::Dirty
    }

    fn make_runnable(&mut self, instr: &[&Instruction]) {
        *self = Hash::blake2b_hash(instr)
            .map(Self::Runnable)
            .unwrap_or(Self::Dirty);
    }
}

/// Interpreted blocks are built automatically, and require no additional context.
#[derive(Debug, Default)]
pub struct InterpretedBlockBuilder;

/// Blocks that are executed via intepreting the individual instructions.
///
/// Interpreted blocks use the [`EnrichedCell`] mechanism, in order to dispatch
/// opcode to function statically during block construction. This saves time over
/// dispatching on every 'instruction run'. See [`ICall`] for more information.
///
/// [`ICall`]: super::ICall
pub struct Interpreted<MC: MemoryConfig, M: ManagerBase> {
    instr: [EnrichedCell<ICallPlaced<MC, M>, M>; CACHE_INSTR],
    len_instr: Cell<u8, M>,
    hash: BlockHash,
}

impl<MC: MemoryConfig, M: ManagerBase> Interpreted<MC, M> {
    /// Calculate the [`BlockHash`] from the instructions in the block.
    ///
    /// If the block is already runnable, it will not re-calculate the block hash.
    fn update_block_hash(&mut self)
    where
        M: ManagerRead,
    {
        let len = self.len_instr.read() as usize;

        let instr = self
            .instr
            .iter()
            .take(len)
            .map(|i| i.read_ref_stored())
            .collect::<Vec<_>>();

        self.hash.make_runnable(&instr)
    }
}

impl<MC: MemoryConfig, M: ManagerBase> BCall<MC, M> for [EnrichedCell<ICallPlaced<MC, M>, M>] {
    #[inline]
    fn num_instr(&self) -> usize
    where
        M: ManagerRead,
    {
        self.len()
    }

    fn run_block(
        &self,
        core: &mut MachineCoreState<MC, M>,
        mut instr_pc: Address,
        steps: &mut usize,
    ) -> Result<(), EnvironException>
    where
        M: ManagerReadWrite,
    {
        if let Err(e) = run_block_inner(self, core, &mut instr_pc, steps) {
            core.handle_step_result(instr_pc, Err(e))?;
            // If we succesfully handled an error, need to increment steps one more.
            *steps += 1;
        }

        Ok(())
    }
}

impl<MC: MemoryConfig, M: ManagerBase> Block<MC, M> for Interpreted<MC, M> {
    type BlockBuilder = InterpretedBlockBuilder;

    fn num_instr(&self) -> usize
    where
        M: ManagerRead,
    {
        self.len_instr.read() as usize
    }

    #[inline]
    fn instr(&self) -> &[EnrichedCell<ICallPlaced<MC, M>, M>]
    where
        M: ManagerRead,
    {
        &self.instr[..self.num_instr()]
    }

    fn invalidate(&mut self)
    where
        M: ManagerWrite,
    {
        self.len_instr.write(0);
        self.hash = BlockHash::Dirty;
    }

    fn push_instr(&mut self, instr: Instruction)
    where
        M: ManagerReadWrite,
    {
        let len = self.len_instr.read();
        self.instr[len as usize].write(instr);
        self.len_instr.write(len + 1);
        self.hash = BlockHash::Dirty;
    }

    fn reset(&mut self)
    where
        M: ManagerReadWrite,
    {
        self.hash = BlockHash::Dirty;
        self.len_instr.write(0);
        self.instr
            .iter_mut()
            .for_each(|lc| lc.write(Instruction::DEFAULT));
    }

    fn start_block(&mut self)
    where
        M: ManagerWrite,
    {
        self.hash = BlockHash::Dirty;
        self.len_instr.write(0);
    }

    fn bind(space: AllocatedOf<BlockLayout, M>) -> Self
    where
        M::ManagerRoot: ManagerReadWrite,
    {
        Self {
            len_instr: space.0,
            instr: space.1.map(EnrichedCell::bind),
            hash: BlockHash::Dirty,
        }
    }

    fn struct_ref<'a, F: FnManager<Ref<'a, M>>>(&'a self) -> AllocatedOf<BlockLayout, F::Output> {
        (
            self.len_instr.struct_ref::<F>(),
            self.instr.each_ref().map(|entry| entry.struct_ref::<F>()),
        )
    }

    /// # SAFETY
    ///
    /// This function is always safe to call.
    #[inline]
    unsafe fn callable<'a>(
        &mut self,
        _bb: &'a mut Self::BlockBuilder,
    ) -> Option<&mut (impl BCall<MC, M> + ?Sized + 'a)>
    where
        M: ManagerRead + 'a,
    {
        let len = self.len_instr.read();
        if len > 0 {
            if self.hash.is_dirty() {
                self.update_block_hash();
            }

            Some(&mut self.instr[0..len as usize])
        } else {
            None
        }
    }

    fn block_hash(&self) -> &BlockHash {
        &self.hash
    }
}

impl<MC: MemoryConfig, M: ManagerClone> Clone for Interpreted<MC, M> {
    fn clone(&self) -> Self {
        Self {
            len_instr: self.len_instr.clone(),
            instr: self.instr.clone(),
            hash: BlockHash::Dirty,
        }
    }
}

/// Blocks that are compiled to native code for execution, when possible.
///
/// Not all instructions are currently supported, when a block contains
/// unsupported instructions, a fallback to [`Interpreted`] mode occurs.
///
/// Blocks are compiled upon calling [`Block::callable`], in a *stop the world* fashion.
pub struct InlineJit<MC: MemoryConfig, M: JitStateAccess> {
    fallback: Interpreted<MC, M>,
    jit_fn: Option<JCall<MC, M>>,
    /// Whether or not compilation has been attempted.
    ///
    /// **N.B.** compilation may fail, in which case `compiled` will still be true, and fallback
    /// should occur to the interpreted block.
    compiled: bool,
}

impl<MC: MemoryConfig, M: JitStateAccess> Block<MC, M> for InlineJit<MC, M> {
    type BlockBuilder = (JIT<MC, M>, InterpretedBlockBuilder);

    fn start_block(&mut self)
    where
        M: ManagerWrite,
    {
        self.compiled = false;
        self.jit_fn = None;
        self.fallback.start_block()
    }

    fn invalidate(&mut self)
    where
        M: ManagerWrite,
    {
        self.compiled = false;
        self.jit_fn = None;
        self.fallback.invalidate()
    }

    fn reset(&mut self)
    where
        M: ManagerReadWrite,
    {
        self.compiled = false;
        self.jit_fn = None;
        self.fallback.reset()
    }

    fn push_instr(&mut self, instr: Instruction)
    where
        M: ManagerReadWrite,
    {
        self.compiled = false;
        self.jit_fn = None;
        self.fallback.push_instr(instr)
    }

    fn instr(&self) -> &[EnrichedCell<ICallPlaced<MC, M>, M>]
    where
        M: ManagerRead,
    {
        self.fallback.instr()
    }

    fn bind(allocated: AllocatedOf<BlockLayout, M>) -> Self {
        Self {
            fallback: Interpreted::bind(allocated),
            jit_fn: None,
            compiled: false,
        }
    }

    fn struct_ref<'a, F: FnManager<Ref<'a, M>>>(&'a self) -> AllocatedOf<BlockLayout, F::Output> {
        self.fallback.struct_ref::<F>()
    }

    /// # SAFETY
    ///
    /// The `block_builder` must be the same as the block builder given to the `compile` call that
    /// (may) have natively compiled this block to machine code.
    ///
    /// This ensures that the builder in question is guaranteed to be alive, for at least as long
    /// as this block may be run via [`BCall::run_block`].
    unsafe fn callable<'a>(
        &mut self,
        block_builder: &'a mut Self::BlockBuilder,
    ) -> Option<&mut (impl BCall<MC, M> + ?Sized + 'a)>
    where
        M: ManagerRead + 'a,
    {
        if self.compiled {
            return Some(self);
        }

        // Trigger hashing of the block, if callable
        self.fallback.callable(&mut block_builder.1);

        let BlockHash::Runnable(hash) = &self.fallback.hash else {
            // Block is not callable
            return None;
        };

        // trigger JIT compilation
        let instr = self
            .fallback
            .instr
            .iter()
            .take(<Self as Block<MC, M>>::num_instr(self))
            .map(|i| i.read_ref_stored());

        let jitfn = block_builder.0.compile(hash, instr);

        self.jit_fn = jitfn;
        self.compiled = true;

        Some(self)
    }

    fn num_instr(&self) -> usize
    where
        M: ManagerRead,
    {
        self.fallback.num_instr()
    }

    fn block_hash(&self) -> &BlockHash {
        &self.fallback.hash
    }
}

impl<MC: MemoryConfig, M: JitStateAccess> BCall<MC, M> for InlineJit<MC, M> {
    fn num_instr(&self) -> usize
    where
        M: ManagerRead,
    {
        self.fallback.num_instr()
    }

    fn run_block(
        &self,
        core: &mut MachineCoreState<MC, M>,
        instr_pc: Address,
        steps: &mut usize,
    ) -> Result<(), EnvironException>
    where
        M: ManagerReadWrite,
    {
        match &self.jit_fn {
            // SAFETY: JIT is guaranteed to be alive here by the caller.
            //         this is due to the only way to run a block being
            //         by calling `Block::callable` first. That function
            //         requires the caller uphold the invariant that
            //         the builder be alive for the lifetime of the
            //         `BCall`.
            Some(jcall) => unsafe { jcall.call(core, instr_pc, steps) },
            None => self.fallback.instr().run_block(core, instr_pc, steps),
        }
    }
}

impl<MC: MemoryConfig, M: JitStateAccess + ManagerClone> Clone for InlineJit<MC, M> {
    fn clone(&self) -> Self {
        Self {
            fallback: self.fallback.clone(),
            jit_fn: None,
            compiled: false,
        }
    }
}

fn run_block_inner<MC: MemoryConfig, M: ManagerReadWrite>(
    instr: &[EnrichedCell<ICallPlaced<MC, M>, M>],
    core: &mut MachineCoreState<MC, M>,
    instr_pc: &mut Address,
    steps: &mut usize,
) -> Result<(), Exception>
where
    M: ManagerReadWrite,
{
    for instr in instr.iter() {
        match run_instr(instr, core) {
            Ok(ProgramCounterUpdate::Next(width)) => {
                *instr_pc += width as u64;
                core.hart.pc.write(*instr_pc);
                *steps += 1;
            }
            Ok(ProgramCounterUpdate::Set(instr_pc)) => {
                // Setting the instr_pc implies execution continuing
                // elsewhere - and no longer within the current block.
                core.hart.pc.write(instr_pc);
                *steps += 1;
                break;
            }
            Err(e) => {
                // Exceptions lead to a new address being set to handle it,
                // with no guarantee of it being the next instruction.
                return Err(e);
            }
        }
    }

    Ok(())
}

#[cfg(test)]
mod test {
    use super::Block;
    use super::BlockHash;
    use super::BlockLayout;
    use super::InlineJit;
    use super::Interpreted;
    use crate::backend_test;
    use crate::create_state;
    use crate::machine_state::instruction::Instruction;
    use crate::machine_state::memory::M4K;
    use crate::machine_state::registers::nz;
    use crate::parser::instruction::InstrWidth;
    use crate::state_backend::test_helpers::TestBackendFactory;

    macro_rules! run_in_block_impl {
        ($F: ty, $block_name:ident, $bb_name:ident, $expr: block) => {{
            type M<F> = <F as TestBackendFactory>::Manager;

            fn inner<B: Block<M4K, M<F>> + Clone, F: TestBackendFactory>(
                $block_name: &mut B,
                $bb_name: &mut <B as Block<M4K, M<F>>>::BlockBuilder,
            ) {
                $expr
            }

            let mut block = create_state!(Interpreted, BlockLayout, $F, M4K);
            let mut bb = <Interpreted<M4K, M<$F>> as Block<M4K, M<$F>>>::BlockBuilder::default();

            inner::<_, $F>(&mut block, &mut bb);

            let mut block = create_state!(InlineJit, BlockLayout, $F, M4K);
            let mut bb = <InlineJit<M4K, M<$F>> as Block<M4K, M<$F>>>::BlockBuilder::default();

            inner::<_, $F>(&mut block, &mut bb);
        }};
    }

    backend_test!(empty_block_not_callable, F, {
        run_in_block_impl!(F, block, bb, {
            assert_eq!(block.num_instr(), 0);
            assert_eq!(block.block_hash(), &BlockHash::Dirty);
            // Safety: block builder alive for the duration of this scope
            assert!(unsafe { block.callable(bb) }.is_none());
            assert_eq!(block.block_hash(), &BlockHash::Dirty);
        });
    });

    backend_test!(block_with_instr_callable, F, {
        run_in_block_impl!(F, block, bb, {
            block.push_instr(Instruction::new_nop(InstrWidth::Compressed));

            // Safety: block builder alive for the duration of this scope
            assert!(unsafe { block.callable(bb) }.is_some());
            assert!(matches!(block.block_hash(), BlockHash::Runnable(_)));
        });
    });

    backend_test!(block_made_dirty_on_clone, F, {
        run_in_block_impl!(F, block, bb, {
            block.push_instr(Instruction::new_nop(InstrWidth::Compressed));

            // Safety: block builder alive for the duration of this scope
            assert!(unsafe { block.callable(bb) }.is_some());

            let new_block = block.clone();
            assert!(matches!(new_block.block_hash(), BlockHash::Dirty));
        });
    });

    backend_test!(block_made_dirty_on_push, F, {
        run_in_block_impl!(F, block, bb, {
            block.push_instr(Instruction::new_nop(InstrWidth::Compressed));
            assert!(matches!(block.block_hash(), BlockHash::Dirty));

            // Safety: block builder alive for the duration of this scope
            assert!(unsafe { block.callable(bb) }.is_some());
            assert!(matches!(block.block_hash(), BlockHash::Runnable(_)));

            // push
            block.push_instr(Instruction::new_nop(InstrWidth::Compressed));
            assert!(matches!(block.block_hash(), BlockHash::Dirty));
        });
    });

    backend_test!(block_hash_unique_for_unique_instructions_sanity, F, {
        run_in_block_impl!(F, block, bb, {
            block.push_instr(Instruction::new_nop(InstrWidth::Compressed));
            block.push_instr(Instruction::new_li(nz::a2, 3, InstrWidth::Compressed));

            // Safety: block builder alive for the duration of this scope
            assert!(unsafe { block.callable(bb) }.is_some());
            let BlockHash::Runnable(hash_1) = block.block_hash() else {
                unreachable!()
            };
            let hash_1 = *hash_1;

            block.reset();
            assert!(matches!(block.block_hash(), BlockHash::Dirty));

            block.push_instr(Instruction::new_nop(InstrWidth::Compressed));

            // Safety: block builder alive for the duration of this scope
            assert!(unsafe { block.callable(bb) }.is_some());
            let BlockHash::Runnable(hash_2) = block.block_hash() else {
                unreachable!()
            };

            assert_ne!(
                hash_1, *hash_2,
                "Hashes for unique sets of instructions must not match"
            );

            block.push_instr(Instruction::new_li(nz::a2, 3, InstrWidth::Compressed));

            // Safety: block builder alive for the duration of this scope
            assert!(unsafe { block.callable(bb) }.is_some());
            let BlockHash::Runnable(hash_3) = block.block_hash() else {
                unreachable!()
            };

            assert_eq!(
                hash_1, *hash_3,
                "Hashes for identical instructions must match"
            );
        });
    });
}
