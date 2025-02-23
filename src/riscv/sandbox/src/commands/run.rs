// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024-2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use crate::{
    cli::{CommonOptions, RunOptions},
    posix_exit_mode,
};
use octez_riscv::{
    machine_state::{DefaultCacheLayouts, main_memory::M1G},
    machine_state::{
        TestCacheLayouts,
        block_cache::bcall::{Block, Interpreted, InterpretedBlockBuilder},
    },
    pvm::PvmHooks,
    state_backend::owned_backend::Owned,
    stepper::{StepResult, Stepper, StepperStatus, pvm::PvmStepper, test::TestStepper},
};
use std::{error::Error, fs, io::Write, ops::Bound};
use tezos_smart_rollup::utils::{console::Console, inbox::InboxBuilder};
use tezos_smart_rollup_encoding::smart_rollup::SmartRollupAddress;

pub fn run(opts: RunOptions) -> Result<(), Box<dyn Error>> {
    let program = fs::read(&opts.input)?;
    let initrd = opts.initrd.as_ref().map(fs::read).transpose()?;

    struct Runner<'a>(&'a RunOptions);

    impl UseStepper<Result<usize, Box<dyn Error>>> for Runner<'_> {
        fn advance<S: Stepper>(self, stepper: S) -> Result<usize, Box<dyn Error>> {
            run_stepper(stepper, self.0.common.max_steps)
        }
    }

    let steps = general_run(&opts.common, program, initrd, Runner(&opts))??;

    if opts.print_steps {
        println!("Run consumed {steps} steps.");
    }

    Ok(())
}

/// XXX: Trait used to pass a function for using the generic stepper.
/// (Couldn't use a trait object + impl FnOnce(...) since [`Stepper`] is not object safe)
pub trait UseStepper<R> {
    fn advance<S: Stepper>(self, stepper: S) -> R;
}

pub fn general_run<F: UseStepper<R>, R>(
    common: &CommonOptions,
    program: Vec<u8>,
    initrd: Option<Vec<u8>>,
    f: F,
) -> Result<R, Box<dyn Error>> {
    let block_builder = InterpretedBlockBuilder;

    if common.pvm {
        run_pvm::<_, Interpreted<_, _>>(
            program.as_slice(),
            initrd.as_deref(),
            common,
            |stepper| f.advance(stepper),
            block_builder,
        )
    } else {
        run_test(
            program.as_slice(),
            initrd.as_deref(),
            common,
            |stepper| f.advance(stepper),
            block_builder,
        )
    }
}

fn run_test<R, B: Block<M1G, Owned>>(
    program: &[u8],
    initrd: Option<&[u8]>,
    common: &CommonOptions,
    f_stepper: impl FnOnce(TestStepper<M1G, TestCacheLayouts, B>) -> R,
    block_builder: B::BlockBuilder,
) -> Result<R, Box<dyn Error>> {
    let stepper = TestStepper::<M1G, _, B>::new(
        program,
        initrd,
        posix_exit_mode(&common.posix_exit_mode),
        block_builder,
    )?;
    Ok(f_stepper(stepper))
}

fn run_pvm<R, B: Block<M1G, Owned>>(
    program: &[u8],
    initrd: Option<&[u8]>,
    common: &CommonOptions,
    f_stepper: impl FnOnce(PvmStepper<M1G, DefaultCacheLayouts, Owned, B>) -> R,
    block_builder: B::BlockBuilder,
) -> Result<R, Box<dyn Error>> {
    let mut inbox = InboxBuilder::new();
    if let Some(inbox_file) = &common.inbox.file {
        inbox.load_from_file(inbox_file)?;
    }

    let rollup_address = SmartRollupAddress::from_b58check(common.inbox.address.as_str())?;

    let mut console = if common.timings {
        Console::with_timings()
    } else {
        Console::new()
    };

    let hooks = PvmHooks::new(|c| {
        let _written = console.write(&[c]).unwrap();
    });

    let stepper = PvmStepper::<'_, M1G, DefaultCacheLayouts, Owned, B>::new(
        program,
        initrd,
        inbox.build(),
        hooks,
        rollup_address.into_hash().as_ref().try_into().unwrap(),
        common.inbox.origination_level,
        block_builder,
    )?;

    Ok(f_stepper(stepper))
}

fn run_stepper(
    mut stepper: impl Stepper,
    max_steps: Option<usize>,
) -> Result<usize, Box<dyn Error>> {
    let max_steps = match max_steps {
        Some(max_steps) => Bound::Included(max_steps),
        None => Bound::Unbounded,
    };

    let result = stepper.step_max(max_steps);

    match result.to_stepper_status() {
        StepperStatus::Exited {
            success: true,
            steps,
            ..
        } => Ok(steps),
        result => Err(format!("{result:?}").into()),
    }
}
