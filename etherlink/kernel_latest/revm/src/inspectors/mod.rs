// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use crate::{
    database::EtherlinkVMDB, precompile_provider::EtherlinkPrecompiles, EVMInnerContext,
    Error,
};
use call_tracer::{CallTracer, CallTracerConfig};
use noop::NoInspector;
use revm::{
    context::{
        result::{EVMError, ExecutionResult, HaltReason},
        ContextSetters, ContextTr, Evm, JournalTr,
    },
    handler::{
        instructions::EthInstructions, EthFrame, EvmTr, EvmTrError, FrameResult, FrameTr,
        Handler,
    },
    inspector::InspectorHandler,
    interpreter::{
        interpreter::{EthInterpreter, ReturnDataImpl},
        interpreter_action::FrameInit,
        interpreter_types::StackTr,
        CallInputs, CallOutcome, CreateInputs, CreateOutcome, InterpreterTypes,
    },
    primitives::B256,
    state::EvmState,
    ExecuteEvm, InspectEvm, Inspector,
};
use tezos_evm_runtime::runtime::Runtime;

pub mod call_tracer;
pub mod noop;
pub mod plain;

mod storage;

pub type EvmInspection<'a, Host> = Evm<
    EVMInnerContext<'a, Host>,
    EtherlinkInspector,
    EthInstructions<EthInterpreter, EVMInnerContext<'a, Host>>,
    EtherlinkPrecompiles,
    EthFrame<EthInterpreter>,
>;

pub struct EtherlinkEvmInspector<'a, Host: Runtime> {
    inner: EvmInspection<'a, Host>,
}

impl<'a, Host: Runtime> ExecuteEvm for EtherlinkEvmInspector<'a, Host> {
    type ExecutionResult = ExecutionResult;
    type State = EvmState;
    type Error = EVMError<Error>;
    type Tx = <EVMInnerContext<'a, Host> as ContextTr>::Tx;
    type Block = <EVMInnerContext<'a, Host> as ContextTr>::Block;

    fn set_block(&mut self, block: Self::Block) {
        self.inner.set_block(block);
    }

    fn transact_one(
        &mut self,
        tx: Self::Tx,
    ) -> Result<Self::ExecutionResult, Self::Error> {
        self.inner.transact_one(tx)
    }

    fn finalize(&mut self) -> Self::State {
        self.inner.finalize()
    }

    fn replay(
        &mut self,
    ) -> Result<
        revm::context::result::ExecResultAndState<Self::ExecutionResult, Self::State>,
        Self::Error,
    > {
        self.inner.replay()
    }
}

#[derive(Debug, Clone)]
pub struct EtherlinkHandler<CTX, ERROR, FRAME> {
    _phantom: core::marker::PhantomData<(CTX, ERROR, FRAME)>,
}

impl<EVM, ERROR, FRAME> Handler for EtherlinkHandler<EVM, ERROR, FRAME>
where
    EVM: EvmTr<Context: ContextTr<Journal: JournalTr<State = EvmState>>, Frame = FRAME>,
    ERROR: EvmTrError<EVM>,
    FRAME: FrameTr<FrameResult = FrameResult, FrameInit = FrameInit>,
{
    type Evm = EVM;
    type Error = ERROR;
    type HaltReason = HaltReason;
}

impl<CTX, ERROR, FRAME> Default for EtherlinkHandler<CTX, ERROR, FRAME> {
    fn default() -> Self {
        Self {
            _phantom: core::marker::PhantomData,
        }
    }
}

impl<EVM, ERROR> InspectorHandler
    for EtherlinkHandler<EVM, ERROR, EthFrame<EthInterpreter>>
where
    EVM: revm::inspector::InspectorEvmTr<
        Context: ContextTr<Journal: JournalTr<State = EvmState>>,
        Frame = EthFrame<EthInterpreter>,
        Inspector: Inspector<<<Self as Handler>::Evm as EvmTr>::Context, EthInterpreter>,
    >,
    ERROR: EvmTrError<EVM>,
{
    type IT = EthInterpreter;
}

impl<Host: Runtime> InspectEvm for EtherlinkEvmInspector<'_, Host> {
    type Inspector = EtherlinkInspector;

    fn set_inspector(&mut self, inspector: Self::Inspector) {
        self.inner.inspector = inspector;
    }

    fn inspect_one_tx(
        &mut self,
        tx: Self::Tx,
    ) -> Result<Self::ExecutionResult, Self::Error> {
        self.inner.set_tx(tx);
        EtherlinkHandler::default().inspect_run(&mut self.inner)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct CallTracerInput {
    pub config: CallTracerConfig,
    pub transaction_hash: Option<B256>,
}

#[derive(Debug, Clone, Copy)]
pub enum TracerInput {
    NoOp,
    CallTracer(CallTracerInput),
}

pub enum EtherlinkInspector {
    NoOp(NoInspector),
    CallTracer(Box<CallTracer>),
}

impl Default for EtherlinkInspector {
    fn default() -> Self {
        Self::NoOp(NoInspector)
    }
}

impl<'a, Host, CTX, INTR> Inspector<CTX, INTR> for EtherlinkInspector
where
    Host: Runtime + 'a,
    CTX: ContextTr<Db = EtherlinkVMDB<'a, Host>>,
    INTR: InterpreterTypes<Stack: StackTr, ReturnData = ReturnDataImpl>,
{
    fn call(
        &mut self,
        context: &mut CTX,
        inputs: &mut CallInputs,
    ) -> Option<CallOutcome> {
        match self {
            Self::NoOp(no_inspector) => {
                <NoInspector as Inspector<CTX, INTR>>::call(no_inspector, context, inputs)
            }
            Self::CallTracer(call_tracer) => {
                <CallTracer as Inspector<CTX, INTR>>::call(call_tracer, context, inputs)
            }
        }
    }

    fn call_end(
        &mut self,
        context: &mut CTX,
        inputs: &CallInputs,
        outcome: &mut CallOutcome,
    ) {
        match self {
            Self::NoOp(no_inspector) => <NoInspector as Inspector<CTX, INTR>>::call_end(
                no_inspector,
                context,
                inputs,
                outcome,
            ),
            Self::CallTracer(call_tracer) => {
                <CallTracer as Inspector<CTX, INTR>>::call_end(
                    call_tracer,
                    context,
                    inputs,
                    outcome,
                )
            }
        }
    }

    fn create(
        &mut self,
        context: &mut CTX,
        inputs: &mut CreateInputs,
    ) -> Option<CreateOutcome> {
        match self {
            Self::NoOp(no_inspector) => <NoInspector as Inspector<CTX, INTR>>::create(
                no_inspector,
                context,
                inputs,
            ),
            Self::CallTracer(call_tracer) => {
                <CallTracer as Inspector<CTX, INTR>>::create(call_tracer, context, inputs)
            }
        }
    }

    fn create_end(
        &mut self,
        context: &mut CTX,
        inputs: &CreateInputs,
        outcome: &mut CreateOutcome,
    ) {
        match self {
            Self::NoOp(no_inspector) => {
                <NoInspector as Inspector<CTX, INTR>>::create_end(
                    no_inspector,
                    context,
                    inputs,
                    outcome,
                )
            }
            Self::CallTracer(call_tracer) => {
                <CallTracer as Inspector<CTX, INTR>>::create_end(
                    call_tracer,
                    context,
                    inputs,
                    outcome,
                )
            }
        }
    }
}
