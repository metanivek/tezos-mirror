// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! A JIT library for compilation of sequences (or blocks) of RISC-V
//! instructions to native code.

mod builder;
pub mod state_access;

use std::collections::HashMap;

use cranelift::codegen::CodegenError;
use cranelift::codegen::ir::types::I64;
use cranelift::codegen::settings::SetError;
use cranelift::frontend::FunctionBuilderContext;
use cranelift::prelude::*;
use cranelift_jit::JITBuilder;
use cranelift_jit::JITModule;
use cranelift_module::Linkage;
use cranelift_module::Module;
use cranelift_module::ModuleError;
use state_access::JitStateAccess;
use thiserror::Error;

use self::builder::Builder;
use self::state_access::JsaCalls;
use self::state_access::JsaImports;
use self::state_access::register_jsa_symbols;
use crate::machine_state::MachineCoreState;
use crate::machine_state::ProgramCounterUpdate;
use crate::machine_state::instruction::Instruction;
use crate::machine_state::memory::MemoryConfig;
use crate::state_backend::hash::Hash;
use crate::traps::EnvironException;

/// Alias for the function signature produced by the JIT compilation.
type JitFn<MC, JSA> = unsafe extern "C" fn(&mut MachineCoreState<MC, JSA>, u64, &mut usize);

/// A jit-compiled function that can be [called] over [`MachineCoreState`].
///
/// [called]: Self::call
pub struct JCall<MC: MemoryConfig, JSA: JitStateAccess> {
    fun: JitFn<MC, JSA>,
}

impl<MC: MemoryConfig, JSA: JitStateAccess> JCall<MC, JSA> {
    /// Run the jit-compiled function over the state.
    ///
    /// # Safety
    ///
    /// When calling, the [JIT] that compiled this function *must*
    /// still be alive.
    pub unsafe fn call(
        &self,
        core: &mut MachineCoreState<MC, JSA>,
        pc: u64,
        steps: &mut usize,
    ) -> Result<(), EnvironException> {
        (self.fun)(core, pc, steps);
        Ok(())
    }
}

impl<MC: MemoryConfig, JSA: JitStateAccess> Clone for JCall<MC, JSA> {
    fn clone(&self) -> Self {
        Self { fun: self.fun }
    }
}

/// Errors that may arise from the initialisation of the JIT.
#[derive(Debug, Error)]
pub enum JitError {
    /// Failures setting flags.
    #[error("Failed to set flag {0}")]
    Setting(#[from] SetError),
    /// Native compilation unsupported on the current arch/os.
    #[error("Native platform unsupported: {0}")]
    UnsupportedPlatform(&'static str),
    /// Constructing the Cranelift builder failed.
    #[error("Unable to initialise builder {0}")]
    BuilderFailure(#[from] CodegenError),
    /// Unable to register external [`JitStateAccess`] functionality.
    #[error("Unable to register external JSA functions: {0}")]
    JsaRegistration(#[from] ModuleError),
}

/// The JIT is responsible for compiling blocks of instructions to machine code,
/// returning a function that can be run over the [`MachineCoreState`].
pub struct JIT<MC: MemoryConfig, JSA: JitStateAccess> {
    /// The function builder context, which is reused across multiple
    /// [`FunctionBuilder`] instances.
    builder_context: FunctionBuilderContext,

    /// The main Cranelift context, which holds the state for codegen. Cranelift
    /// separates this from `Module` to allow for parallel compilation, with a
    /// context per thread, though this isn't in the simple demo here.
    ctx: codegen::Context,

    /// The module, with the jit backend, which manages the JIT'd
    /// functions.
    module: JITModule,

    /// Imported [JitStateAccess] functions.
    jsa_imports: JsaImports<MC, JSA>,

    /// Cache of compilation results.
    cache: HashMap<Hash, Option<JCall<MC, JSA>>>,
}

impl<MC: MemoryConfig, JSA: JitStateAccess> JIT<MC, JSA> {
    /// Create a new instance of the JIT, which will be able to
    /// produce functions that can be run over the current
    /// memory configuration and manager.
    pub fn new() -> Result<Self, JitError> {
        let mut flag_builder = settings::builder();
        flag_builder.set("use_colocated_libcalls", "false")?;
        flag_builder.set("is_pic", "false")?;

        let isa_builder = cranelift_native::builder().map_err(JitError::UnsupportedPlatform)?;
        let isa = isa_builder.finish(settings::Flags::new(flag_builder))?;

        let mut builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());
        register_jsa_symbols::<MC, JSA>(&mut builder);

        let mut module = JITModule::new(builder);
        let jsa_imports = JsaImports::declare_in_module(&mut module)?;

        Ok(Self {
            builder_context: FunctionBuilderContext::new(),
            ctx: codegen::Context::new(),
            module,
            jsa_imports,
            cache: Default::default(),
        })
    }

    /// Compile a sequence of instructions to a callable native function.
    ///
    /// Not all instructions are currently supported. For blocks containing
    /// unsupported instructions, `None` will be returned.
    pub fn compile<'a>(
        &mut self,
        hash: &Hash,
        instr: impl IntoIterator<Item = &'a Instruction>,
    ) -> Option<JCall<MC, JSA>> {
        if let Some(compilation_result) = self.cache.get(hash) {
            return compilation_result.clone();
        }

        let mut builder = self.start();

        for i in instr {
            let Some(lower) = i.opcode.to_lowering() else {
                builder.fail();
                self.clear();
                return None;
            };

            let pc_update = unsafe {
                // # SAFETY: lower is called with args from the same instruction that it
                // was derived
                (lower)(i.args(), &mut builder)
            };

            builder.steps += 1;
            match pc_update {
                ProgramCounterUpdate::Next(width) => {
                    builder.pc_offset += width as u64;
                }
                ProgramCounterUpdate::Set(pc_val) => {
                    builder.pc_offset = 0;
                    builder.pc_val = pc_val;
                    break;
                }
            }
        }

        builder.end();

        let name = hex::encode(hash);

        let fun = self.finalise(&name);
        let jcall = JCall { fun };

        self.cache.insert(*hash, Some(jcall.clone()));

        Some(JCall { fun })
    }

    /// Setup the builder, ensuring the entry block of the function is correct.
    ///
    /// # Input Args
    ///
    /// | `core: &mut MachineCoreState` | `int (ptr) -> MachineCoreState` |
    /// | `pc: Address`                 | `I64`                           |
    /// | `steps: &mut usize`           | `int (ptr) -> int`              |
    ///
    /// # Return
    ///
    /// | `steps: usize`                | `int`                           |
    fn start(&mut self) -> Builder<'_, MC, JSA> {
        let ptr = self.module.target_config().pointer_type();

        self.ctx.func.signature.params.push(AbiParam::new(ptr));
        self.ctx.func.signature.params.push(AbiParam::new(I64));
        self.ctx.func.signature.params.push(AbiParam::new(ptr));

        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);

        // Create the entry block, to start emitting code in.
        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        let core_ptr_val = builder.block_params(entry_block)[0];
        let pc_val = builder.block_params(entry_block)[1];
        let steps_ptr_val = builder.block_params(entry_block)[2];

        let jsa_call = JsaCalls::func_calls(&mut self.module, &self.jsa_imports);

        Builder::<'_, MC, JSA> {
            builder,
            ptr,
            core_ptr_val,
            steps_ptr_val,
            steps: 0,
            pc_val,
            pc_offset: 0,
            jsa_call,
        }
    }

    /// Finalise the function currently under construction.
    fn finalise(&mut self, name: &str) -> JitFn<MC, JSA> {
        let id = self
            .module
            .declare_function(name.as_ref(), Linkage::Export, &self.ctx.func.signature)
            .map_err(|e| e.to_string())
            .unwrap();

        // define the function to jit
        self.module.define_function(id, &mut self.ctx).unwrap();

        // finalise the function
        self.module.finalize_definitions().unwrap();
        let code = self.module.get_finalized_function(id);

        self.clear();

        // SAFETY: the signature of a JitFn matches exactly the abi we specified in the
        //         entry block. Compilation has succeeded & therefore this produced code
        //         is safe to call.
        unsafe { std::mem::transmute(code) }
    }

    /// Clear the current context to allow a new function to be compiled
    fn clear(&mut self) {
        self.module.clear_context(&mut self.ctx)
    }
}

// TODO: https://linear.app/tezos/issue/RV-496
//       `Block::BlockBuilder` should not require Default, as it
//         does not allow for potential fallilibility
impl<MC: MemoryConfig, M: JitStateAccess> Default for JIT<MC, M> {
    fn default() -> Self {
        Self::new().expect("JIT is supported on all octez-riscv supported platforms")
    }
}

#[cfg(test)]
mod tests {
    use std::i64;

    use Instruction as I;

    use super::*;
    use crate::backend_test;
    use crate::create_state;
    use crate::machine_state::MachineCoreState;
    use crate::machine_state::MachineCoreStateLayout;
    use crate::machine_state::block_cache::bcall::BCall;
    use crate::machine_state::block_cache::bcall::Block;
    use crate::machine_state::block_cache::bcall::BlockLayout;
    use crate::machine_state::block_cache::bcall::Interpreted;
    use crate::machine_state::block_cache::bcall::InterpretedBlockBuilder;
    use crate::machine_state::memory::M4K;
    use crate::machine_state::memory::MemoryConfig;
    use crate::parser::instruction::InstrWidth::*;
    use crate::state_backend::FnManagerIdent;
    use crate::state_backend::ManagerRead;
    use crate::state_backend::test_helpers::TestBackendFactory;
    use crate::state_backend::test_helpers::assert_eq_struct;

    fn instructions<MC: MemoryConfig, M>(block: &Interpreted<MC, M>) -> Vec<Instruction>
    where
        M: ManagerRead,
    {
        let instr = block.instr();
        instr.iter().map(|cell| cell.read_stored()).collect()
    }

    type SetupHook<F> = dyn Fn(&mut MachineCoreState<M4K, <F as TestBackendFactory>::Manager>);
    type AssertHook<F> = dyn Fn(&MachineCoreState<M4K, <F as TestBackendFactory>::Manager>);

    struct Scenario<F: TestBackendFactory> {
        initial_pc: Option<u64>,
        expected_steps: Option<usize>,
        instructions: Vec<Instruction>,
        setup_hook: Option<Box<SetupHook<F>>>,
        assert_hook: Option<Box<AssertHook<F>>>,
    }

    impl<F: TestBackendFactory> Scenario<F> {
        fn simple(instructions: &[Instruction]) -> Self {
            Scenario {
                initial_pc: None,
                expected_steps: None,
                instructions: instructions.to_vec(),
                setup_hook: None,
                assert_hook: None,
            }
        }

        /// Run a test scenario over both the Interpreted & JIT modes of compilation,
        /// to ensure they behave identically.
        fn run(
            &self,
            jit: &mut JIT<M4K, F::Manager>,
            interpreted_bb: &mut InterpretedBlockBuilder,
        ) {
            // Create the states for the interpreted and jitted runs.
            let mut interpreted =
                create_state!(MachineCoreState, MachineCoreStateLayout<M4K>, F, M4K);
            let mut jitted = create_state!(MachineCoreState, MachineCoreStateLayout<M4K>, F, M4K);

            let hash = super::Hash::blake2b_hash(&self.instructions).unwrap();

            // Create the block of instructions.
            let mut block = create_state!(Interpreted, BlockLayout, F, M4K);
            block.start_block();
            for instr in self.instructions.iter() {
                block.push_instr(*instr);
            }

            // Run the setup hooks.
            if let Some(hook) = &self.setup_hook {
                (hook)(&mut interpreted);
                (hook)(&mut jitted)
            }

            // initialise starting parameters: pc, steps
            let initial_pc = self.initial_pc.unwrap_or_default();
            let mut interpreted_steps = 0;
            let mut jitted_steps = 0;
            interpreted.hart.pc.write(initial_pc);
            jitted.hart.pc.write(initial_pc);

            // Create the JIT function.
            let fun = jit
                .compile(&hash, instructions(&block).as_slice())
                .expect("Compilation of block should succeed.");

            // Run the block in both interpreted and jitted mode.
            let interpreted_res = unsafe {
                // SAFETY: interpreted blocks are always callable
                block.callable(interpreted_bb)
            }
            .unwrap()
            .run_block(&mut interpreted, initial_pc, &mut interpreted_steps);
            let jitted_res = unsafe {
                // # Safety - the block builder is alive for at least
                //            the duration of the `run` function.
                fun.call(&mut jitted, initial_pc, &mut jitted_steps)
            };

            // Assert state equality.
            assert_eq!(jitted_res, interpreted_res);
            assert_eq!(
                interpreted_steps, jitted_steps,
                "Interpreted mode ran for {interpreted_steps}, compared to jit-mode of {jitted_steps}"
            );
            assert_eq_struct(
                &interpreted.struct_ref::<FnManagerIdent>(),
                &jitted.struct_ref::<FnManagerIdent>(),
            );

            // Only check steps against one state, as we know both interpreted/jit steps are equal.
            let expected_steps = self.expected_steps.unwrap_or(self.instructions.len());
            assert_eq!(
                interpreted_steps, expected_steps,
                "Scenario ran for {interpreted_steps} steps, but expected {expected_steps}"
            );

            // Run the assert hooks. Since we have already verified that the states are equal,
            // we can run the assert hooks on just the interpreted state.
            if let Some(hook) = &self.assert_hook {
                (hook)(&mut interpreted);
            }
        }
    }

    /// A builder for creating scenarios.
    struct ScenarioBuilder<F: TestBackendFactory> {
        initial_pc: Option<u64>,
        expected_steps: Option<usize>,
        instructions: Vec<Instruction>,
        setup_hook: Option<Box<SetupHook<F>>>,
        assert_hook: Option<Box<AssertHook<F>>>,
    }

    impl<F: TestBackendFactory> Default for ScenarioBuilder<F> {
        fn default() -> Self {
            ScenarioBuilder {
                initial_pc: None,
                expected_steps: None,
                instructions: Vec::new(),
                setup_hook: None,
                assert_hook: None,
            }
        }
    }

    impl<F: TestBackendFactory> ScenarioBuilder<F> {
        fn set_instructions(mut self, instructions: &[Instruction]) -> Self {
            self.instructions = instructions.to_vec();
            self
        }

        fn set_initial_pc(mut self, initial_pc: u64) -> Self {
            self.initial_pc = Some(initial_pc);
            self
        }

        fn set_expected_steps(mut self, expected_steps: usize) -> Self {
            self.expected_steps = Some(expected_steps);
            self
        }

        fn set_assert_hook(mut self, assert_hook: Box<AssertHook<F>>) -> Self {
            self.assert_hook = Some(assert_hook);
            self
        }

        fn build(self) -> Scenario<F> {
            Scenario {
                initial_pc: self.initial_pc,
                expected_steps: self.expected_steps,
                instructions: self.instructions,
                setup_hook: self.setup_hook,
                assert_hook: self.assert_hook,
            }
        }
    }

    macro_rules! assert_hook {
        ($core:ident, $F:ident, $block:expr) => {
            Box::new(move |$core: &MachineCoreState<M4K, $F::Manager>| $block)
        };
    }

    backend_test!(test_cnop, F, {
        let scenarios: &[Scenario<F>] = &[
            Scenario::simple(&[I::new_nop(Compressed)]),
            Scenario::simple(&[I::new_nop(Compressed), I::new_nop(Uncompressed)]),
            Scenario::simple(&[
                I::new_nop(Uncompressed),
                I::new_nop(Compressed),
                I::new_nop(Uncompressed),
            ]),
        ];

        let mut jit = JIT::<M4K, F::Manager>::new().unwrap();
        let mut interpreted_bb = InterpretedBlockBuilder;

        for scenario in scenarios {
            scenario.run(&mut jit, &mut interpreted_bb);
        }
    });

    backend_test!(test_cmv, F, {
        use crate::machine_state::registers::NonZeroXRegister::*;

        let assert_x2_is_one = assert_hook!(core, F, {
            assert_eq!(core.hart.xregisters.read_nz(x2), 1);
        });

        // Arrange
        let scenarios: &[Scenario<F>] = &[
            ScenarioBuilder::default()
                .set_instructions(&[I::new_li(x1, 1, Compressed), I::new_mv(x2, x1, Compressed)])
                .set_assert_hook(assert_x2_is_one.clone())
                .build(),
            ScenarioBuilder::default()
                .set_instructions(&[
                    I::new_li(x1, 1, Uncompressed),
                    I::new_mv(x2, x1, Uncompressed),
                ])
                .set_assert_hook(assert_x2_is_one.clone())
                .build(),
            ScenarioBuilder::default()
                .set_instructions(&[
                    I::new_li(x1, 1, Compressed),
                    I::new_mv(x2, x1, Compressed),
                    I::new_mv(x3, x2, Compressed),
                ])
                .set_assert_hook(assert_x2_is_one)
                .build(),
        ];

        let mut jit = JIT::<M4K, F::Manager>::new().unwrap();
        let mut interpreted_bb = InterpretedBlockBuilder;

        for scenario in scenarios {
            scenario.run(&mut jit, &mut interpreted_bb);
        }
    });

    backend_test!(test_negate, F, {
        use Instruction as I;

        use crate::machine_state::registers::NonZeroXRegister::*;

        let assert_x1_x2_equal = assert_hook!(core, F, {
            assert_eq!(
                core.hart.xregisters.read_nz(x1),
                core.hart.xregisters.read_nz(x2)
            );
        });

        let scenarios: &[Scenario<F>] = &[
            ScenarioBuilder::default()
                .set_instructions(&[
                    I::new_li(x1, -1, Compressed),
                    I::new_li(x3, 1, Compressed),
                    I::new_neg(x2, x3, Compressed),
                ])
                .set_assert_hook(assert_x1_x2_equal.clone())
                .build(),
            ScenarioBuilder::default()
                .set_instructions(&[
                    I::new_li(x1, 1, Uncompressed),
                    I::new_neg(x3, x1, Uncompressed),
                    I::new_neg(x2, x3, Compressed),
                ])
                .set_assert_hook(assert_x1_x2_equal)
                .build(),
            ScenarioBuilder::default()
                .set_instructions(&[
                    I::new_li(x1, i64::MIN, Uncompressed),
                    I::new_neg(x2, x1, Uncompressed),
                ])
                .set_assert_hook(assert_hook!(core, F, {
                    assert_eq!(core.hart.xregisters.read_nz(x2), i64::MIN as u64);
                }))
                .build(),
        ];

        let mut jit = JIT::<M4K, F::Manager>::new().unwrap();
        let mut interpreted_bb = InterpretedBlockBuilder;

        for scenario in scenarios {
            scenario.run(&mut jit, &mut interpreted_bb);
        }
    });

    backend_test!(test_add, F, {
        use crate::machine_state::registers::NonZeroXRegister::*;

        let assert_x1_is_five = assert_hook!(core, F, {
            assert_eq!(core.hart.xregisters.read_nz(x1), 5);
        });

        let scenario: Scenario<F> = ScenarioBuilder::default()
            .set_instructions(&[
                I::new_li(x1, 1, Uncompressed),
                I::new_add(x2, x2, x1, Compressed),
                I::new_add(x1, x1, x2, Uncompressed),
                I::new_add(x2, x2, x1, Uncompressed),
                I::new_add(x1, x1, x2, Compressed),
            ])
            .set_assert_hook(assert_x1_is_five)
            .build();

        let mut jit = JIT::<M4K, F::Manager>::new().unwrap();
        let mut interpreted_bb = InterpretedBlockBuilder;

        scenario.run(&mut jit, &mut interpreted_bb);
    });

    backend_test!(test_sub, F, {
        use Instruction as I;

        use crate::machine_state::registers::NonZeroXRegister::*;

        let scenarios: &[Scenario<F>] = &[
            ScenarioBuilder::default()
                .set_instructions(&[
                    I::new_li(x1, 10, Uncompressed),
                    I::new_sub(x2, x1, x1, Compressed),
                ])
                .set_assert_hook(assert_hook!(core, F, {
                    assert_eq!(core.hart.xregisters.read_nz(x2), 0);
                }))
                .build(),
            ScenarioBuilder::default()
                .set_instructions(&[
                    I::new_li(x1, 10, Compressed),
                    I::new_li(x3, -10, Uncompressed),
                    I::new_sub(x2, x1, x3, Uncompressed),
                ])
                .set_assert_hook(assert_hook!(core, F, {
                    assert_eq!(core.hart.xregisters.read_nz(x2), 20);
                }))
                .build(),
            ScenarioBuilder::default()
                .set_instructions(&[
                    I::new_li(x1, 10, Uncompressed),
                    I::new_li(x3, 100, Compressed),
                    I::new_sub(x2, x1, x3, Compressed),
                ])
                .set_assert_hook(assert_hook!(core, F, {
                    assert_eq!(core.hart.xregisters.read_nz(x2), (-90_i64) as u64);
                }))
                .build(),
        ];

        let mut jit = JIT::<M4K, F::Manager>::new().unwrap();
        let mut interpreted_bb = InterpretedBlockBuilder;

        for scenario in scenarios {
            scenario.run(&mut jit, &mut interpreted_bb);
        }
    });

    backend_test!(test_and, F, {
        use crate::machine_state::registers::NonZeroXRegister::*;

        let assert_x1_and_x2_equal = assert_hook!(core, F, {
            assert_eq!(
                core.hart.xregisters.read_nz(x1),
                core.hart.xregisters.read_nz(x2)
            );
        });

        let scenarios: &[Scenario<F>] = &[
            ScenarioBuilder::default()
                // Bitwise and with all ones is self.
                .set_instructions(&[
                    I::new_li(x1, 13872, Uncompressed),
                    I::new_li(x3, !0, Compressed),
                    I::new_and(x2, x1, x3, Compressed),
                ])
                .set_assert_hook(assert_x1_and_x2_equal.clone())
                .build(),
            ScenarioBuilder::default()
                // Bitwise and with itself is self.
                .set_instructions(&[
                    I::new_li(x1, 49666, Uncompressed),
                    I::new_and(x2, x1, x1, Compressed),
                ])
                .set_assert_hook(assert_x1_and_x2_equal.clone())
                .build(),
            ScenarioBuilder::default()
                // Bitwise and with 0 is 0.
                .set_instructions(&[
                    I::new_li(x1, 0, Uncompressed),
                    I::new_li(x3, 540921, Compressed),
                    I::new_and(x2, x1, x3, Compressed),
                ])
                .set_assert_hook(assert_x1_and_x2_equal)
                .build(),
        ];

        let mut jit = JIT::<M4K, F::Manager>::new().unwrap();
        let mut interpreted_bb = InterpretedBlockBuilder;

        for scenario in scenarios {
            scenario.run(&mut jit, &mut interpreted_bb);
        }
    });

    backend_test!(test_or, F, {
        use crate::machine_state::registers::NonZeroXRegister::*;

        let assert_x1_and_x2_equal = assert_hook!(core, F, {
            assert_eq!(
                core.hart.xregisters.read_nz(x1),
                core.hart.xregisters.read_nz(x2)
            );
        });

        let scenarios: &[Scenario<F>] = &[
            // Bitwise or with all ones is all-ones.
            ScenarioBuilder::default()
                .set_instructions(&[
                    I::new_li(x1, !0, Uncompressed),
                    I::new_li(x3, 13872, Compressed),
                    I::new_or(x2, x1, x3, Compressed),
                ])
                .set_assert_hook(assert_x1_and_x2_equal.clone())
                .build(),
            // Bitwise or with itself is self.
            ScenarioBuilder::default()
                .set_instructions(&[
                    I::new_li(x1, 49666, Uncompressed),
                    I::new_or(x2, x1, x1, Compressed),
                ])
                .set_assert_hook(assert_x1_and_x2_equal.clone())
                .build(),
            // Bitwise or with 0 is self.
            ScenarioBuilder::default()
                .set_instructions(&[
                    I::new_li(x1, 540921, Uncompressed),
                    I::new_li(x3, 0, Compressed),
                    I::new_or(x2, x1, x3, Compressed),
                ])
                .set_assert_hook(assert_x1_and_x2_equal)
                .build(),
        ];

        let mut jit = JIT::<M4K, F::Manager>::new().unwrap();
        let mut interpreted_bb = InterpretedBlockBuilder;

        for scenario in scenarios {
            scenario.run(&mut jit, &mut interpreted_bb);
        }
    });

    backend_test!(test_j, F, {
        let scenarios: &[Scenario<F>] = &[
            ScenarioBuilder::default()
                // Jumping to the next instruction should exit the block
                .set_instructions(&[
                    I::new_nop(Compressed),
                    I::new_nop(Compressed),
                    I::new_j(2, Compressed),
                ])
                .set_assert_hook(assert_hook!(core, F, {
                    assert_eq!(core.hart.pc.read(), 6);
                }))
                .set_expected_steps(3)
                .build(),
            ScenarioBuilder::default()
                // Jump past 0 - in both worlds we should wrap around.
                .set_instructions(&[I::new_j(-4, Compressed)])
                .set_assert_hook(assert_hook!(core, F, {
                    assert_eq!(core.hart.pc.read(), u64::MAX - 3);
                }))
                .set_expected_steps(1)
                .build(),
            ScenarioBuilder::default()
                // Jump past u64::MAX - in both worlds we should wrap around but not
                // execute functions past the end of the block (the jump).
                .set_instructions(&[
                    I::new_nop(Uncompressed),
                    I::new_nop(Uncompressed),
                    I::new_j(i64::MAX, Uncompressed),
                    I::new_nop(Compressed),
                    I::new_nop(Uncompressed),
                ])
                .set_initial_pc((i64::MAX - 5) as u64)
                .set_assert_hook(assert_hook!(core, F, {
                    assert_eq!(core.hart.pc.read(), 1);
                }))
                .set_expected_steps(3)
                .build(),
            ScenarioBuilder::default()
                // jump by nothing
                .set_instructions(&[
                    I::new_nop(Compressed),
                    I::new_j(0, Compressed),
                    I::new_nop(Uncompressed),
                ])
                .set_assert_hook(assert_hook!(core, F, {
                    assert_eq!(core.hart.pc.read(), 2);
                }))
                .set_expected_steps(2)
                .build(),
            ScenarioBuilder::default()
                // jumping to start of the block should exit the block in both interpreted and jitted world
                .set_instructions(&[
                    I::new_nop(Compressed),
                    I::new_nop(Compressed),
                    I::new_j(-4, Compressed),
                    I::new_nop(Uncompressed),
                ])
                .set_assert_hook(assert_hook!(core, F, {
                    assert_eq!(core.hart.pc.read(), 0);
                }))
                .set_expected_steps(3)
                .build(),
        ];

        let mut jit = JIT::<M4K, F::Manager>::new().unwrap();
        let mut interpreted_bb = InterpretedBlockBuilder;

        for scenario in scenarios {
            scenario.run(&mut jit, &mut interpreted_bb);
        }
    });

    backend_test!(test_jr, F, {
        use crate::machine_state::registers::NonZeroXRegister::*;

        let scenarios: &[Scenario<F>] = &[
            ScenarioBuilder::default()
                // Jumping should exit the block
                .set_instructions(&[
                    I::new_li(x2, 10, Compressed),
                    I::new_jr(x2, Compressed),
                    I::new_nop(Compressed),
                ])
                .set_assert_hook(assert_hook!(core, F, {
                    assert_eq!(core.hart.pc.read(), 10);
                }))
                .set_expected_steps(2)
                .build(),
            ScenarioBuilder::default()
                // Jumping to start of the block should still exit.
                .set_instructions(&[I::new_li(x6, 0, Compressed), I::new_jr(x6, Compressed)])
                .set_assert_hook(assert_hook!(core, F, {
                    assert_eq!(core.hart.pc.read(), 0);
                }))
                .set_expected_steps(2)
                .build(),
        ];

        let mut jit = JIT::<M4K, F::Manager>::new().unwrap();
        let mut interpreted_bb = InterpretedBlockBuilder;

        for scenario in scenarios {
            scenario.run(&mut jit, &mut interpreted_bb);
        }
    });

    backend_test!(test_jr_imm, F, {
        use crate::machine_state::registers::NonZeroXRegister::*;

        let scenarios: &[Scenario<F>] = &[
            ScenarioBuilder::default()
                // Jumping to the next instruction should exit the block
                .set_instructions(&[
                    I::new_li(x2, 10, Uncompressed),
                    I::new_jr_imm(x2, 10, Compressed),
                    I::new_nop(Uncompressed),
                ])
                .set_assert_hook(assert_hook!(core, F, {
                    assert_eq!(core.hart.pc.read(), 20);
                }))
                .set_expected_steps(2)
                .build(),
            ScenarioBuilder::default()
                // Jumping to start of the block should still exit.
                .set_instructions(&[
                    I::new_li(x6, 10, Compressed),
                    I::new_jr_imm(x6, -10, Uncompressed),
                ])
                .set_assert_hook(assert_hook!(core, F, {
                    assert_eq!(core.hart.pc.read(), 0);
                }))
                .set_expected_steps(2)
                .build(),
        ];

        let mut jit = JIT::<M4K, F::Manager>::new().unwrap();
        let mut interpreted_bb = InterpretedBlockBuilder;

        for scenario in scenarios {
            scenario.run(&mut jit, &mut interpreted_bb);
        }
    });

    backend_test!(test_addi, F, {
        use Instruction as I;

        use crate::machine_state::registers::NonZeroXRegister::*;

        let assert_x1_is_five = assert_hook!(core, F, {
            assert_eq!(core.hart.xregisters.read_nz(x1), 5);
        });

        let scenarios: &[Scenario<F>] = &[
            ScenarioBuilder::default()
                .set_instructions(&[
                    I::new_addi(x1, x1, 2, Compressed),
                    I::new_addi(x1, x1, 3, Uncompressed),
                ])
                .set_assert_hook(assert_x1_is_five.clone())
                .build(),
            ScenarioBuilder::default()
                .set_instructions(&[
                    I::new_addi(x1, x1, i64::MAX, Compressed),
                    I::new_addi(x1, x1, i64::MAX, Compressed),
                    I::new_addi(x1, x1, 7, Uncompressed),
                ])
                .set_assert_hook(assert_x1_is_five.clone())
                .build(),
            ScenarioBuilder::default()
                .set_instructions(&[
                    I::new_addi(x1, x3, 7, Compressed),
                    I::new_addi(x1, x1, -2, Uncompressed),
                ])
                .set_assert_hook(assert_x1_is_five)
                .build(),
        ];

        let mut jit = JIT::<M4K, F::Manager>::new().unwrap();
        let mut interpreted_bb = InterpretedBlockBuilder;

        for scenario in scenarios {
            scenario.run(&mut jit, &mut interpreted_bb);
        }
    });

    backend_test!(test_andi, F, {
        use Instruction as I;

        use crate::machine_state::registers::NonZeroXRegister::*;

        let assert_x1_and_x2_equal = assert_hook!(core, F, {
            assert_eq!(
                core.hart.xregisters.read_nz(x1),
                core.hart.xregisters.read_nz(x2)
            );
        });

        let scenarios: &[Scenario<F>] = &[
            // Bitwise and with all ones is self.
            ScenarioBuilder::default()
                .set_instructions(&[
                    I::new_li(x1, 13872, Uncompressed),
                    I::new_andi(x2, x1, !0, Compressed),
                ])
                .set_assert_hook(assert_x1_and_x2_equal.clone())
                .build(),
            // Bitwise and with itself is self.
            ScenarioBuilder::default()
                .set_instructions(&[
                    I::new_li(x1, 49666, Uncompressed),
                    I::new_andi(x2, x1, 49666, Compressed),
                ])
                .set_assert_hook(assert_x1_and_x2_equal.clone())
                .build(),
            // Bitwise and with 0 is 0.
            ScenarioBuilder::default()
                .set_instructions(&[
                    I::new_li(x1, 0, Uncompressed),
                    I::new_andi(x2, x1, 50230, Compressed),
                ])
                .set_assert_hook(assert_x1_and_x2_equal)
                .build(),
        ];

        let mut jit = JIT::<M4K, F::Manager>::new().unwrap();
        let mut interpreted_bb = InterpretedBlockBuilder;

        for scenario in scenarios {
            scenario.run(&mut jit, &mut interpreted_bb);
        }
    });

    backend_test!(test_jit_recovers_from_compilation_failure, F, {
        use crate::machine_state::registers::NonZeroXRegister::*;

        // Arrange
        let failure_scenarios: &[&[I]] = &[
            &[
                // does not currently support lowering.
                I::new_xori(x1, x1, 13, Uncompressed),
            ],
            &[
                I::new_nop(Uncompressed),
                // does not currently support lowering.
                I::new_xori(x1, x1, 13, Uncompressed),
            ],
        ];

        let success: &[I] = &[I::new_nop(Compressed)];
        let success_hash = super::Hash::blake2b_hash(success).unwrap();

        for failure in failure_scenarios.iter() {
            let mut jit = JIT::<M4K, F::Manager>::new().unwrap();

            let mut jitted = create_state!(MachineCoreState, MachineCoreStateLayout<M4K>, F, M4K);
            let mut block = create_state!(Interpreted, BlockLayout, F, M4K);
            let failure_hash = super::Hash::blake2b_hash(failure).unwrap();

            block.start_block();
            for instr in failure.iter() {
                block.push_instr(*instr);
            }

            let mut jitted_steps = 0;

            let initial_pc = 0;
            jitted.hart.pc.write(initial_pc);

            jitted.hart.xregisters.write_nz(x1, 1);

            // Act
            let res = jit.compile(&failure_hash, instructions(&block).as_slice());

            assert!(
                res.is_none(),
                "Compilation of unsupported instruction should fail"
            );

            block.start_block();
            for instr in success.iter() {
                block.push_instr(*instr);
            }

            let fun = jit
                .compile(&success_hash, instructions(&block).as_slice())
                .expect("Compilation of subsequent functions should succeed");
            let jitted_res = unsafe {
                // # Safety - the jit is not dropped until after we
                //            exit the block.
                fun.call(&mut jitted, initial_pc, &mut jitted_steps)
            };

            assert!(jitted_res.is_ok());
            assert_eq!(jitted_steps, success.len());
        }
    });
}
