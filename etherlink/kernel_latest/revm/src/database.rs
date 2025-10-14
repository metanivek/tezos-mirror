// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use crate::{
    custom,
    helpers::legacy::FaDepositWithProxy,
    journal::PrecompileStateChanges,
    precompiles::{error::CustomPrecompileError, send_outbox_message::Withdrawal},
    storage::{
        block::{get_block_hash, BLOCKS_STORED},
        code::CodeStorage,
        sequencer_key_change::store_sequencer_key_change,
        world_state_handler::{
            StorageAccount, GOVERNANCE_SEQUENCER_UPGRADE_PATH, KT1_B58_SIZE,
            SEQUENCER_KEY_PATH, WITHDRAWALS_TICKETER_PATH,
        },
    },
    Error,
};
use revm::{
    primitives::{Address, HashMap, StorageKey, StorageValue, B256, KECCAK_EMPTY, U256},
    state::{Account, AccountInfo, Bytecode, EvmStorage, EvmStorageSlot},
    Database, DatabaseCommit,
};
use tezos_crypto_rs::{
    hash::{ContractKt1Hash, HashTrait},
    public_key::PublicKey,
};
use tezos_ethereum::block::BlockConstants;
use tezos_evm_logging::{log, tracing::instrument, Level};
use tezos_evm_runtime::runtime::Runtime;
use tezos_smart_rollup_host::runtime::RuntimeError;

pub struct EtherlinkVMDB<'a, Host: Runtime> {
    /// Runtime host
    pub host: &'a mut Host,
    /// Constants for the current block
    block: &'a BlockConstants,
    /// Commit guard, the `DatabaseCommit` trait and in particular
    /// its `commit` function does NOT return errors.
    /// We need this guard to change if there's an unrecoverable
    /// error and we need to revert the changes made to the durable
    /// storage.
    commit_status: bool,
    /// Withdrawals accumulated by the current execution and consumed at the end of it
    withdrawals: Vec<Withdrawal>,
    /// HACK: [PRECOMPILE_ZERO_ADDRESS_AND_SIMULATION]
    /// This is used in order to avoid the problem of EIP-3607 for address
    /// zero which contains the forwarder code.
    caller: Address,
    /// Storage access to address zero aka the system address
    system: StorageAccount,
    /// Account info snapshot when read to avoid re-write them if they haven't change and only
    /// the storage has been touched
    original_account_infos: HashMap<Address, AccountInfo>,
}

enum AccountState {
    Touched((AccountInfo, EvmStorage)),
    SelfDestructed,
}

impl<'a, Host: Runtime> EtherlinkVMDB<'a, Host> {
    #[instrument(skip_all)]
    pub fn new(
        host: &'a mut Host,
        block: &'a BlockConstants,
        caller: Address,
    ) -> Result<Self, Error> {
        let system = StorageAccount::from_address(&Address::ZERO)?;
        Ok(EtherlinkVMDB {
            host,
            block,
            commit_status: true,
            withdrawals: vec![],
            caller,
            system,
            original_account_infos: HashMap::default(),
        })
    }
}

pub trait DatabasePrecompileStateChanges {
    fn log_node_message(&mut self, level: Level, message: &str);
    fn global_counter(&self) -> Result<U256, CustomPrecompileError>;
    fn ticket_balance(
        &self,
        ticket_hash: &U256,
        owner: &Address,
    ) -> Result<U256, CustomPrecompileError>;
    fn sequencer(&self) -> Result<PublicKey, CustomPrecompileError>;
    fn governance_sequencer_upgrade_exists(&self) -> Result<bool, CustomPrecompileError>;
    fn deposit_in_queue(
        &self,
        deposit_id: &U256,
    ) -> Result<FaDepositWithProxy, CustomPrecompileError>;
    fn ticketer(&self) -> Result<ContractKt1Hash, CustomPrecompileError>;
}

pub(crate) trait DatabaseCommitPrecompileStateChanges {
    fn commit(&mut self, etherlink_data: PrecompileStateChanges);
}

macro_rules! abort_on_error {
    ($obj:expr, $expr:expr, $msg:expr) => {
        if let Err(err) = $expr {
            $obj.abort();
            log!($obj.host, Level::Error, "{} error: {err:?}", $msg);
            return;
        }
    };
}

impl<Host: Runtime> EtherlinkVMDB<'_, Host> {
    #[instrument(skip_all)]
    pub fn commit_status(&self) -> bool {
        self.commit_status
    }

    pub fn take_withdrawals(&mut self) -> Vec<Withdrawal> {
        std::mem::take(&mut self.withdrawals)
    }

    fn abort(&mut self) {
        self.commit_status = false;
    }

    fn update_account(&mut self, address: Address, account_state: AccountState) {
        match StorageAccount::from_address(&address) {
            Ok(mut storage_account) => match account_state {
                AccountState::Touched((mut info, storage)) => {
                    if let Some(code) = info.code.take() {
                        abort_on_error!(
                            self,
                            CodeStorage::add(
                                self.host,
                                code.original_byte_slice(),
                                Some(info.code_hash)
                            ),
                            "DatabaseCommit `CodeStorage::add`"
                        );
                    }
                    // Avoid rewriting the account info if it hasn't changed
                    if self.original_account_infos.get(&address) != Some(&info) {
                        abort_on_error!(
                            self,
                            storage_account.set_info_without_code(self.host, info),
                            "DatabaseCommit `set_info_without_code`"
                        );
                    }

                    for (
                        key,
                        EvmStorageSlot {
                            original_value,
                            present_value,
                            ..
                        },
                    ) in storage
                    {
                        if original_value != present_value {
                            abort_on_error!(
                                self,
                                storage_account.set_storage(
                                    self.host,
                                    &key,
                                    &present_value,
                                ),
                                "DatabaseCommit `set_storage`"
                            );
                        }
                    }
                }
                AccountState::SelfDestructed => {
                    abort_on_error!(
                        self,
                        storage_account
                            .set_info_without_code(self.host, AccountInfo::default()),
                        "DatabaseCommit `set_info_without_code`"
                    );
                }
            },
            error => {
                abort_on_error!(self, error, "DatabaseCommit `get_or_create_account`")
            }
        }
    }
}

// Precompile read functions care about the difference between a path not found and a runtime error
// as path not found is the only one that will produce a revert result
impl<Host: Runtime> DatabasePrecompileStateChanges for EtherlinkVMDB<'_, Host> {
    fn log_node_message(&mut self, level: Level, message: &str) {
        log!(self.host, level, "{message:?}");
    }

    fn global_counter(&self) -> Result<U256, CustomPrecompileError> {
        Ok(self.system.read_global_counter(self.host)?)
    }

    fn ticket_balance(
        &self,
        ticket_hash: &U256,
        owner: &Address,
    ) -> Result<U256, CustomPrecompileError> {
        Ok(self
            .system
            .read_ticket_balance(self.host, ticket_hash, owner)?)
    }

    fn deposit_in_queue(
        &self,
        deposit_id: &U256,
    ) -> Result<FaDepositWithProxy, CustomPrecompileError> {
        Ok(self.system.read_deposit_from_queue(self.host, deposit_id)?)
    }

    fn ticketer(&self) -> Result<ContractKt1Hash, CustomPrecompileError> {
        let ticketer =
            self.host
                .store_read(&WITHDRAWALS_TICKETER_PATH, 0, KT1_B58_SIZE)?;
        let kt1_b58 = String::from_utf8(ticketer.to_vec()).map_err(custom)?;
        Ok(ContractKt1Hash::from_b58check(&kt1_b58).map_err(custom)?)
    }

    fn sequencer(&self) -> Result<PublicKey, CustomPrecompileError> {
        let bytes = self.host.internal_store_read_all(&SEQUENCER_KEY_PATH)?;
        PublicKey::from_b58check(std::str::from_utf8(&bytes).map_err(|e| {
            CustomPrecompileError::Revert(format!(
                "Invalid sequencer key UTF-8 encoding: {e}"
            ))
        })?)
        .map_err(|e| {
            CustomPrecompileError::Revert(format!(
                "Invalid sequencer key Base58Check encoding: {e}"
            ))
        })
    }

    fn governance_sequencer_upgrade_exists(&self) -> Result<bool, CustomPrecompileError> {
        match self
            .host
            .internal_store_read_all(&GOVERNANCE_SEQUENCER_UPGRADE_PATH)
        {
            Ok(_) => Ok(true),
            Err(RuntimeError::PathNotFound) => Ok(false),
            Err(e) => Err(CustomPrecompileError::from(e)),
        }
    }
}

impl<Host: Runtime> Database for EtherlinkVMDB<'_, Host> {
    type Error = Error;

    fn basic(&mut self, address: Address) -> Result<Option<AccountInfo>, Self::Error> {
        let storage_account = StorageAccount::from_address(&address)?;
        let account_info = storage_account.info(self.host)?;

        self.original_account_infos
            .insert(address, account_info.copy_without_code());

        if self.caller == Address::ZERO && address == Address::ZERO {
            // HACK: [PRECOMPILE_ZERO_ADDRESS_AND_SIMULATION]
            // This can only happen in a simulation case only.
            // The zero address contains code for legacy reasons related to precompiles.
            // We must return empty code here otherwise the simulation will fail because
            // of EIP-3607.
            return Ok(Some(AccountInfo {
                code_hash: KECCAK_EMPTY,
                code: None,
                ..account_info
            }));
        }

        Ok(Some(account_info))
    }

    fn code_by_hash(&mut self, code_hash: B256) -> Result<Bytecode, Self::Error> {
        let code_storage = CodeStorage::new(&code_hash)?;
        let bytecode = code_storage.get_code(self.host)?;
        Ok(bytecode.unwrap_or_default())
    }

    fn storage(
        &mut self,
        address: Address,
        index: StorageKey,
    ) -> Result<StorageValue, Self::Error> {
        let storage_account = StorageAccount::from_address(&address)?;
        storage_account.get_storage(self.host, &index)
    }

    fn block_hash(&mut self, number: u64) -> Result<B256, Self::Error> {
        // return 0 when block number not in valid range
        // Ref. https://www.evm.codes/?fork=prague#40 (opcode 0x40)

        match self.block.number.checked_sub(number.into()) {
            Some(block_diff)
                if block_diff <= BLOCKS_STORED.into() && !block_diff.is_zero() =>
            {
                get_block_hash(self.host, number)
            }
            _ => Ok(B256::ZERO),
        }
    }
}
impl<Host: Runtime> DatabaseCommitPrecompileStateChanges for EtherlinkVMDB<'_, Host> {
    fn commit(&mut self, etherlink_data: PrecompileStateChanges) {
        self.withdrawals = etherlink_data.withdrawals.into_iter().collect();
        if let Some(new_sequencer_key_change) = etherlink_data.sequencer_key_change {
            abort_on_error!(
                self,
                store_sequencer_key_change(self.host, new_sequencer_key_change),
                "DatabaseCommitPrecompileStateChanges `store_sequencer_key_change`"
            );
        }
        if let Some(global_counter) = etherlink_data.global_counter {
            abort_on_error!(
                self,
                self.system.write_global_counter(self.host, global_counter),
                "DatabaseCommitPrecompileStateChanges `write_global_counter`"
            );
        }
        for ((owner, ticket_hash), amount) in etherlink_data.ticket_balances {
            abort_on_error!(
                self,
                self.system
                    .write_ticket_balance(self.host, &ticket_hash, &owner, amount),
                "DatabaseCommitPrecompileStateChanges `write_ticket_balance`"
            );
        }
        for (deposit_id, deposit) in etherlink_data.deposits.iter() {
            abort_on_error!(
                self,
                self.system.write_deposit(self.host, deposit_id, deposit),
                "DatabaseCommitPrecompileStateChanges `write_deposit`"
            );
        }
        for deposit_id in etherlink_data.removed_deposits {
            abort_on_error!(
                self,
                self.system
                    .remove_deposit_from_queue(self.host, &deposit_id),
                "DatabaseCommitPrecompileStateChanges `remove_deposit_from_queue`"
            );
        }
    }
}

impl<Host: Runtime> DatabaseCommit for EtherlinkVMDB<'_, Host> {
    fn commit(&mut self, changes: HashMap<Address, Account>) {
        for (address, account) in changes {
            // The account isn't marked as touched, the changes are not commited
            // to the database.
            if !account.is_touched() {
                continue;
            }

            // The account is touched and marked as selfdestructed, we clear the
            // account.
            if account.is_selfdestructed() {
                self.update_account(address, AccountState::SelfDestructed);
                continue;
            }

            // The account is touched, the changes are naturally commited to the database.
            self.update_account(
                address,
                AccountState::Touched((account.info, account.storage)),
            );
        }
        self.original_account_infos.clear();
    }
}
