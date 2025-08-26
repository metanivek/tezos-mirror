// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use revm::{
    context::{Cfg, ContextTr, LocalContextTr},
    handler::{EthPrecompiles, PrecompileProvider},
    interpreter::{CallInput, Gas, InputsImpl, InstructionResult, InterpreterResult},
    primitives::{Address, Bytes},
};

use crate::{
    database::PrecompileDatabase,
    precompiles::constants::{
        CUSTOMS, SEND_OUTBOX_MESSAGE_PRECOMPILE_ADDRESS, TABLE_PRECOMPILE_ADDRESS,
    },
    precompiles::send_outbox_message::send_outbox_message_precompile,
    precompiles::table::table_precompile,
};

#[derive(Debug, Default, Clone)]
pub struct EtherlinkPrecompiles {
    pub builtins: EthPrecompiles,
}

impl EtherlinkPrecompiles {
    pub fn new() -> Self {
        Self {
            builtins: EthPrecompiles::default(),
        }
    }

    fn warm_addresses(&self) -> Box<impl Iterator<Item = Address>> {
        Box::new(self.builtins.warm_addresses().chain(CUSTOMS))
    }

    fn contains(&self, address: &Address) -> bool {
        CUSTOMS.contains(address) || self.builtins.contains(address)
    }

    fn run_custom_precompile<CTX>(
        &mut self,
        context: &mut CTX,
        address: &Address,
        inputs: &InputsImpl,
        is_static: bool,
        gas_limit: u64,
    ) -> Result<Option<InterpreterResult>, String>
    where
        CTX: ContextTr,
        CTX::Db: PrecompileDatabase,
    {
        // NIT: can probably do this more efficiently by keeping an immutable
        // reference on the slice but next mutable call makes it nontrivial
        let input_bytes = match &inputs.input {
            CallInput::SharedBuffer(range) => {
                if let Some(slice) =
                    context.local().shared_memory_buffer_slice(range.clone())
                {
                    slice.to_vec()
                } else {
                    vec![]
                }
            }
            CallInput::Bytes(bytes) => bytes.to_vec(),
        };

        match *address {
            SEND_OUTBOX_MESSAGE_PRECOMPILE_ADDRESS => {
                let result = send_outbox_message_precompile(
                    &input_bytes,
                    context,
                    is_static,
                    inputs,
                    gas_limit,
                );
                let interpreter_res = result.unwrap_or_else(|e| revert(&e.to_string()));
                Ok(Some(interpreter_res))
            }
            TABLE_PRECOMPILE_ADDRESS => {
                let result = table_precompile(
                    &input_bytes,
                    context,
                    is_static,
                    inputs,
                    gas_limit,
                )?;
                Ok(Some(result))
            }
            _ => Ok(None),
        }
    }
}

impl<CTX> PrecompileProvider<CTX> for EtherlinkPrecompiles
where
    CTX: ContextTr,
    CTX::Db: PrecompileDatabase,
{
    type Output = InterpreterResult;

    fn set_spec(&mut self, spec: <CTX::Cfg as Cfg>::Spec) -> bool {
        <EthPrecompiles as PrecompileProvider<CTX>>::set_spec(&mut self.builtins, spec)
    }

    fn run(
        &mut self,
        context: &mut CTX,
        address: &Address,
        inputs: &InputsImpl,
        is_static: bool,
        gas_limit: u64,
    ) -> Result<Option<Self::Output>, String> {
        if let Some(custom_result) =
            self.run_custom_precompile(context, address, inputs, is_static, gas_limit)?
        {
            return Ok(Some(custom_result));
        }

        self.builtins
            .run(context, address, inputs, is_static, gas_limit)
    }

    fn warm_addresses(&self) -> Box<impl Iterator<Item = Address>> {
        self.warm_addresses()
    }

    fn contains(&self, address: &Address) -> bool {
        self.contains(address)
    }
}

pub(crate) fn revert(reason: &str) -> InterpreterResult {
    InterpreterResult {
        result: InstructionResult::Revert,
        gas: Gas::new(0),
        output: Bytes::copy_from_slice(reason.as_bytes()),
    }
}
