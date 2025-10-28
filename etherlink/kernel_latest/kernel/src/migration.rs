// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024 Trilitech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use crate::block_storage;
use crate::blueprint_storage::{
    blueprint_path, clear_all_blueprints, store_current_block_header,
};
use crate::chains::ETHERLINK_SAFE_STORAGE_ROOT_PATH;
use crate::error::Error;
use crate::error::StorageError;
use crate::error::UpgradeProcessError;
use crate::migration::legacy::{
    account_path, init_account_storage, ENABLE_FAST_FA_WITHDRAWAL,
    ENABLE_FAST_WITHDRAWAL, NATIVE_TOKEN_TICKETER_PATH, SYSTEM_ACCOUNT_ADDRESS,
    WITHDRAWAL_ADDRESS,
};
use crate::storage::{
    read_chain_id, read_storage_version, store_backlog, store_dal_slots,
    store_storage_version, tweak_dal_activation, StorageVersion, DELAYED_BRIDGE,
    ENABLE_FA_BRIDGE, KERNEL_GOVERNANCE, KERNEL_SECURITY_GOVERNANCE,
    SEQUENCER_GOVERNANCE,
};
use primitive_types::U256;
use revm_etherlink::storage::block::BLOCKS_STORED;
use revm_etherlink::storage::version::{store_evm_version, EVMVersion};
use tezos_evm_logging::{log, Level::*};
use tezos_evm_runtime::runtime::Runtime;
use tezos_smart_rollup::storage::path::RefPath;
use tezos_smart_rollup_host::path::OwnedPath;
use tezos_smart_rollup_host::runtime::RuntimeError;

#[derive(Eq, PartialEq)]
pub enum MigrationStatus {
    None,
    InProgress,
    Done,
}

// /!\ the following functions are migratin helpers, do not remove them /!\

#[allow(dead_code)]
const TESTNET_CHAIN_ID: u64 = 128123;

#[allow(dead_code)]
const MAINNET_CHAIN_ID: u64 = 42793;

#[allow(dead_code)]
fn is_etherlink_network(
    host: &impl Runtime,
    expected_chain_id: u64,
) -> Result<bool, Error> {
    match read_chain_id(host) {
        Ok(chain_id) => Ok(chain_id == expected_chain_id.into()),
        Err(Error::Storage(StorageError::Runtime(RuntimeError::PathNotFound))) => {
            Ok(false)
        }
        Err(err) => Err(err),
    }
}

#[allow(dead_code)]
pub fn allow_path_not_found(res: Result<(), RuntimeError>) -> Result<(), RuntimeError> {
    match res {
        Ok(()) => Ok(()),
        Err(RuntimeError::PathNotFound) => Ok(()),
        Err(err) => Err(err),
    }
}

const TMP_NEXT_BLUEPRINT_PATH: RefPath =
    RefPath::assert_from(b"/__tmp_next_blueprint_path");

mod legacy {
    // This module contains copies of old implementations of some
    // functions. The legacy semantics of these functions is needed in
    // some migration step to access the storage using the fields
    // which were present at the time.

    use super::*;
    use primitive_types::H160;
    use revm::state::AccountInfo;
    use revm_etherlink::{
        helpers::legacy::{alloy_to_u256, u256_to_alloy},
        storage::world_state_handler::StorageAccount,
    };
    use tezos_smart_rollup_storage::storage::Storage;
    use tezos_storage::error::Error as GenStorageError;
    use thiserror::Error;

    pub fn read_next_blueprint_number<Host: Runtime>(
        host: &Host,
    ) -> anyhow::Result<U256> {
        match block_storage::read_current_number(host, &ETHERLINK_SAFE_STORAGE_ROOT_PATH)
        {
            Err(err) => match err.downcast_ref() {
                Some(GenStorageError::Runtime(RuntimeError::PathNotFound)) => {
                    Ok(U256::zero())
                }
                _ => Err(err),
            },
            Ok(block_number) => Ok(block_number.saturating_add(U256::one())),
        }
    }

    // System (zero) account address, owns ticket table and withdrawal counter
    pub const SYSTEM_ACCOUNT_ADDRESS: H160 = H160::zero();

    // Prefixed by 'ff' to make sure we will not conflict with any
    // upcoming Ethereum upgrades.
    pub const WITHDRAWAL_ADDRESS: H160 = H160([
        0xff, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
    ]);

    pub const ENABLE_FAST_WITHDRAWAL: RefPath =
        RefPath::assert_from(b"/evm/world_state/feature_flags/enable_fast_withdrawal");

    pub const NATIVE_TOKEN_TICKETER_PATH: RefPath =
        RefPath::assert_from(b"/evm/world_state/ticketer");

    pub const ENABLE_FAST_FA_WITHDRAWAL: RefPath =
        RefPath::assert_from(b"/evm/world_state/feature_flags/enable_fast_fa_withdrawal");

    pub const EVM_ACCOUNTS_PATH: RefPath =
        RefPath::assert_from(b"/evm/world_state/eth_accounts");

    pub type EthereumAccountStorage = Storage<EthereumAccount>;

    #[derive(Debug, PartialEq)]
    pub struct EthereumAccount {
        pub path: OwnedPath,
    }

    impl From<OwnedPath> for EthereumAccount {
        fn from(path: OwnedPath) -> Self {
            Self { path }
        }
    }

    impl EthereumAccount {
        /// Increment the **nonce** by one. It is technically possible for this operation to overflow,
        /// but in practice this will not happen for a very long time. The nonce is a 256 bit unsigned
        /// integer.
        pub fn increment_nonce(
            &mut self,
            host: &mut impl Runtime,
        ) -> Result<(), AccountStorageError> {
            let mut old_info = self.info(host)?;

            let new_value = old_info
                .nonce
                .checked_add(1)
                .ok_or(AccountStorageError::NonceOverflow)?;
            old_info.nonce = new_value;
            self.set_info_without_code(host, old_info)
        }

        /// Get the **balance** of an account in Wei held by the account.
        pub fn balance(
            &self,
            host: &mut impl Runtime,
        ) -> Result<U256, AccountStorageError> {
            let new_format_account = StorageAccount::from_path(self.path.clone());
            Ok(alloy_to_u256(&new_format_account.info(host)?.balance))
        }

        /// Remove an amount in Wei from the balance of an account. If the account doesn't hold
        /// enough funds, this will underflow, in which case the account is unaffected, but the
        /// function call will return `Ok(false)`. In case the removal went without underflow,
        /// ie the account held enough funds, the function returns `Ok(true)`.
        pub fn balance_remove(
            &mut self,
            host: &mut impl Runtime,
            amount: U256,
        ) -> Result<bool, AccountStorageError> {
            let mut old_info = self.info(host)?;
            if let Some(new_value) = old_info.balance.checked_sub(u256_to_alloy(&amount))
            {
                old_info.balance = new_value;
                self.set_info_without_code(host, old_info)?;
                Ok(true)
            } else {
                Ok(false)
            }
        }

        pub fn set_info_without_code(
            &mut self,
            host: &mut impl Runtime,
            info: AccountInfo,
        ) -> Result<(), AccountStorageError> {
            let mut new_format_account = StorageAccount::from_path(self.path.clone());
            new_format_account
                .set_info_without_code(host, info)
                .map_err(AccountStorageError::from)
        }

        pub fn info(
            &self,
            host: &mut impl Runtime,
        ) -> Result<AccountInfo, AccountStorageError> {
            let new_format_account = StorageAccount::from_path(self.path.clone());
            new_format_account
                .info(host)
                .map_err(AccountStorageError::from)
        }
    }

    #[derive(Error, Clone, Copy, Debug, Eq, PartialEq)]
    pub enum DurableStorageError {
        /// Some runtime error happened while using durable storage
        #[error("Runtime error: {0:?}")]
        RuntimeError(#[from] tezos_smart_rollup_host::runtime::RuntimeError),
        /// Some error happened while constructing the path to some
        /// resource.
        #[error("Path error: {0:?}")]
        PathError(#[from] tezos_smart_rollup_host::path::PathError),
    }

    /// All errors that may happen as result of using the Ethereum account
    /// interface.
    #[derive(Error, Eq, PartialEq, Clone, Debug)]
    pub enum AccountStorageError {
        #[error("Transaction storage API error: {0:?}")]
        StorageError(tezos_smart_rollup_storage::StorageError),
        #[error("REVM Storage error: {0}")]
        REVMStorageError(revm_etherlink::Error),
        /// Technically, the Ethereum account nonce can overflow if
        /// an account does an incredible number of transactions.
        #[error("Nonce overflow")]
        NonceOverflow,
    }

    impl From<tezos_smart_rollup_storage::StorageError> for AccountStorageError {
        fn from(error: tezos_smart_rollup_storage::StorageError) -> Self {
            AccountStorageError::StorageError(error)
        }
    }

    impl From<revm_etherlink::Error> for AccountStorageError {
        fn from(error: revm_etherlink::Error) -> Self {
            AccountStorageError::REVMStorageError(error)
        }
    }

    pub fn init_account_storage() -> Result<EthereumAccountStorage, AccountStorageError> {
        Storage::<EthereumAccount>::init(&EVM_ACCOUNTS_PATH)
            .map_err(AccountStorageError::from)
    }

    /// Turn an Ethereum address - a H160 - into a valid path
    pub fn account_path(address: &H160) -> Result<OwnedPath, DurableStorageError> {
        let path_string = alloc::format!("/{}", hex::encode(address.to_fixed_bytes()));
        OwnedPath::try_from(path_string).map_err(DurableStorageError::from)
    }
}

fn migrate_to<Host: Runtime>(
    host: &mut Host,
    version: StorageVersion,
) -> anyhow::Result<MigrationStatus> {
    log!(host, Info, "Migrating to {:?}", version);
    match version {
        StorageVersion::V11 => anyhow::bail!(Error::UpgradeError(
            UpgradeProcessError::InternalUpgrade("V11 has no predecessor"),
        )),
        StorageVersion::V12 => {
            let legacy_ticketer_path = RefPath::assert_from(b"/evm/ticketer");
            if host.store_has(&legacy_ticketer_path)?.is_some() {
                host.store_move(&legacy_ticketer_path, &NATIVE_TOKEN_TICKETER_PATH)?;
            }

            Ok(MigrationStatus::Done)
        }
        StorageVersion::V13 => Ok(MigrationStatus::Done),
        StorageVersion::V14 => {
            if is_etherlink_network(host, TESTNET_CHAIN_ID)? {
                host.store_write_all(&ENABLE_FA_BRIDGE, &[1u8])?;
                Ok(MigrationStatus::Done)
            } else {
                // Not applicable for other networks
                Ok(MigrationStatus::None)
            }
        }
        StorageVersion::V15 => {
            // Starting version 15, the entrypoint `populate_delayed_inbox`
            // is available.
            Ok(MigrationStatus::Done)
        }
        StorageVersion::V16 => {
            // Allow path not found in case the migration is performed
            // on a context with no blocks or no transactions.
            allow_path_not_found(host.store_delete(&RefPath::assert_from(
                b"/evm/world_state/indexes/accounts",
            )))?;
            allow_path_not_found(host.store_delete(&RefPath::assert_from(
                b"/evm/world_state/indexes/transactions",
            )))?;
            // Starting version 16, the `callTracer` configuration is available
            // for tracing.
            Ok(MigrationStatus::Done)
        }
        StorageVersion::V17 => {
            // Starting version 17 the kernel no longer needs all transactions
            // in its storage to produce the receipts and transactions root.
            Ok(MigrationStatus::Done)
        }
        StorageVersion::V18 => {
            // Blocks were indexed twice in the storage.
            // [/evm/world_state/indexes/blocks] is the mapping of all block
            // numbers to hashes.
            // [/evm/world_state/blocks/<number>/hash] is the mapgping of the
            // last 256 blocks to hashes
            //
            // We need only the former.

            let current_number = block_storage::read_current_number(
                host,
                &ETHERLINK_SAFE_STORAGE_ROOT_PATH,
            )?;
            let to_clean = U256::min(current_number + 1, BLOCKS_STORED.into());
            for i in 0..to_clean.as_usize() {
                let number = current_number - i;
                let path: Vec<u8> =
                    format!("/evm/world_state/blocks/{number}/hash").into();
                let owned_path = OwnedPath::try_from(path)?;
                host.store_delete(&owned_path)?;
            }
            Ok(MigrationStatus::Done)
        }
        StorageVersion::V19 => {
            // We do not support EIP161 yet. If we start doing it, we
            // might clean the zero account by accident, and the account
            // must always exist as the ticket table is stored in the
            // zero address.
            let account_storage = init_account_storage()?;
            let account_path = account_path(&SYSTEM_ACCOUNT_ADDRESS)?;
            let mut account = account_storage.get_or_create(host, &account_path)?;
            account.increment_nonce(host)?;
            Ok(MigrationStatus::Done)
        }
        StorageVersion::V20 => {
            let account_storage = init_account_storage()?;
            let mut withdrawal_precompiled = account_storage
                .get_or_create(host, &account_path(&WITHDRAWAL_ADDRESS)?)?;
            let balance = withdrawal_precompiled.balance(host)?;
            if !balance.is_zero() {
                withdrawal_precompiled.balance_remove(host, balance)?;
            }
            Ok(MigrationStatus::Done)
        }
        StorageVersion::V21 => {
            if is_etherlink_network(host, MAINNET_CHAIN_ID)? {
                host.store_write_all(
                    &DELAYED_BRIDGE,
                    b"KT1Vocor3bL5ZSgsYH9ztt42LNhqFK64soR4",
                )?;
                Ok(MigrationStatus::Done)
            } else if is_etherlink_network(host, TESTNET_CHAIN_ID)? {
                host.store_write_all(
                    &DELAYED_BRIDGE,
                    b"KT1X1M4ywyz9cHvUgBLTUUdz3GTiYJhPcyPh",
                )?;
                Ok(MigrationStatus::Done)
            } else {
                // Not applicable for other networks
                Ok(MigrationStatus::None)
            }
        }
        StorageVersion::V22 => {
            if is_etherlink_network(host, MAINNET_CHAIN_ID)? {
                host.store_write_all(&ENABLE_FA_BRIDGE, &[1u8])?;
                Ok(MigrationStatus::Done)
            } else {
                // Not applicable for other networks
                Ok(MigrationStatus::None)
            }
        }
        StorageVersion::V23 => {
            // Clear all the blueprints, we accumulated a lot of old
            // blueprints without cleaning them.
            //
            // As we remove everything that means the sequencer will
            // have to republish some.
            //
            // However we need to keep the next blueprint as it
            // trigerred the upgrade.

            let next_blueprint_number = legacy::read_next_blueprint_number(host)?;
            let blueprint_path = blueprint_path(next_blueprint_number)?;
            allow_path_not_found(
                host.store_move(&blueprint_path, &TMP_NEXT_BLUEPRINT_PATH),
            )?;
            clear_all_blueprints(host)?;
            allow_path_not_found(
                host.store_move(&TMP_NEXT_BLUEPRINT_PATH, &blueprint_path),
            )?;
            Ok(MigrationStatus::Done)
        }
        StorageVersion::V24 => {
            const EVM_BASE_FEE_PER_GAS: RefPath =
                RefPath::assert_from(b"/evm/world_state/fees/base_fee_per_gas");
            host.store_delete(&EVM_BASE_FEE_PER_GAS)?;
            Ok(MigrationStatus::Done)
        }
        StorageVersion::V25 => {
            if is_etherlink_network(host, MAINNET_CHAIN_ID)? {
                const REGULAR_GOVERNANCE_KT: &[u8] =
                    b"KT1FPG4NApqTJjwvmhWvqA14m5PJxu9qgpBK";
                const SECURITY_GOVERNANCE_KT: &[u8] =
                    b"KT1GRAN26ni19mgd6xpL6tsH52LNnhKSQzP2";
                const SEQUENCER_GOVERNANCE_KT: &[u8] =
                    b"KT1UvCsnXpLAssgeJmrbQ6qr3eFkYXxsTG9U";

                host.store_write_all(&KERNEL_GOVERNANCE, REGULAR_GOVERNANCE_KT)?;
                host.store_write_all(
                    &KERNEL_SECURITY_GOVERNANCE,
                    SECURITY_GOVERNANCE_KT,
                )?;
                host.store_write_all(&SEQUENCER_GOVERNANCE, SEQUENCER_GOVERNANCE_KT)?;

                Ok(MigrationStatus::Done)
            } else {
                Ok(MigrationStatus::None)
            }
        }
        StorageVersion::V26 => {
            host.store_write_all(&ENABLE_FAST_WITHDRAWAL, &[1_u8])?;
            Ok(MigrationStatus::Done)
        }
        StorageVersion::V27 => {
            // Initialize the next_blueprint_info field
            match block_storage::read_current(
                host,
                &ETHERLINK_SAFE_STORAGE_ROOT_PATH,
                &crate::chains::ChainFamily::Evm,
            ) {
                Ok(block) => {
                    store_current_block_header(host, &block.into())?;
                    Ok(MigrationStatus::Done)
                }
                Err(err) => match err.downcast_ref() {
                    Some(tezos_storage::error::Error::Runtime(
                        RuntimeError::PathNotFound,
                    )) => Ok(MigrationStatus::Done),
                    _ => Err(err),
                },
            }
        }
        StorageVersion::V28 => {
            if is_etherlink_network(host, MAINNET_CHAIN_ID)?
                || is_etherlink_network(host, TESTNET_CHAIN_ID)?
            {
                store_evm_version(host, &EVMVersion::Cancun)?;
            }
            Ok(MigrationStatus::Done)
        }
        StorageVersion::V29 => {
            /// Path used to store to the number of timestamps read.
            const EVM_INFO_PER_LEVEL_STATS_NUMBERS: RefPath =
                RefPath::assert_from(b"/evm/info_per_level/stats/numbers");
            /// Path used to store the sum of distance between blocks.
            const EVM_INFO_PER_LEVEL_STATS_TOTAL: RefPath =
                RefPath::assert_from(b"/evm/info_per_level/stats/total");
            host.store_delete(&EVM_INFO_PER_LEVEL_STATS_NUMBERS)?;
            host.store_delete(&EVM_INFO_PER_LEVEL_STATS_TOTAL)?;
            Ok(MigrationStatus::Done)
        }
        StorageVersion::V30 => {
            host.store_write_all(&ENABLE_FAST_FA_WITHDRAWAL, &[1_u8])?;
            Ok(MigrationStatus::Done)
        }
        StorageVersion::V31 => {
            if is_etherlink_network(host, MAINNET_CHAIN_ID)?
                || is_etherlink_network(host, TESTNET_CHAIN_ID)?
            {
                tweak_dal_activation(host, true)?;
                // We allow 8 slots in order to have around twice the size of the inbox.
                // NB:
                // * One slot is 127kb.
                // * The size of the inbox is 512kb.
                store_dal_slots(host, &[0, 1, 2, 3, 4, 5, 6, 7])?
            }
            Ok(MigrationStatus::Done)
        }
        StorageVersion::V32 => {
            // We clear the gas price backlog. This is because the backlog’s unit has changed from
            // ticks to gas, and there is easily a factor 1,000 between the two. Clearing backlog
            // will prevent an undesired gas price spike at migration time.
            store_backlog(host, 0)?;

            Ok(MigrationStatus::Done)
        }
        StorageVersion::V33 => {
            if is_etherlink_network(host, MAINNET_CHAIN_ID)? {
                const REGULAR_GOVERNANCE_KT: &[u8] =
                    b"KT1XdSAYGXrUDE1U5GNqUKKscLWrMhzyjNeh";
                const SECURITY_GOVERNANCE_KT: &[u8] =
                    b"KT1D1fRgZVdjTj5sUZKcSTPPnuR7LRxVYnDL";
                const SEQUENCER_GOVERNANCE_KT: &[u8] =
                    b"KT1NnH9DCAoY1pfPNvb9cw9XPKQnHAFYFHXa";

                host.store_write_all(&KERNEL_GOVERNANCE, REGULAR_GOVERNANCE_KT)?;
                host.store_write_all(
                    &KERNEL_SECURITY_GOVERNANCE,
                    SECURITY_GOVERNANCE_KT,
                )?;
                host.store_write_all(&SEQUENCER_GOVERNANCE, SEQUENCER_GOVERNANCE_KT)?;

                Ok(MigrationStatus::Done)
            } else {
                Ok(MigrationStatus::None)
            }
        }
        StorageVersion::V34 => {
            // Dummy migration allowing to version-gate the gas limit
            // validation in the EVM node
            Ok(MigrationStatus::Done)
        }
        StorageVersion::V35 => {
            // Dummy migration allowing the node to decide what gas target to use when trying to
            // predict the gas price
            Ok(MigrationStatus::Done)
        }
        StorageVersion::V36 => {
            if is_etherlink_network(host, MAINNET_CHAIN_ID)? {
                const REGULAR_GOVERNANCE_KT: &[u8] =
                    b"KT1VZVNCNnhUp7s15d9RsdycP7C1iwYhAQ8r";
                const SECURITY_GOVERNANCE_KT: &[u8] =
                    b"KT1DxndcFitAbxLdJCN3C1pPivqbC3RJxD1R";
                const SEQUENCER_GOVERNANCE_KT: &[u8] =
                    b"KT1WckZ2uiLfHCfQyNp1mtqeRcC1X6Jg2Qzf";

                host.store_write_all(&KERNEL_GOVERNANCE, REGULAR_GOVERNANCE_KT)?;
                host.store_write_all(
                    &KERNEL_SECURITY_GOVERNANCE,
                    SECURITY_GOVERNANCE_KT,
                )?;
                host.store_write_all(&SEQUENCER_GOVERNANCE, SEQUENCER_GOVERNANCE_KT)?;

                Ok(MigrationStatus::Done)
            } else {
                Ok(MigrationStatus::None)
            }
        }
        StorageVersion::V37 => {
            store_evm_version(host, &EVMVersion::Prague)?;
            Ok(MigrationStatus::Done)
        }
        StorageVersion::V38 => {
            // Dummy migration for the new gas price
            Ok(MigrationStatus::Done)
        }
        // ///////////////////////////////////////////////////////////////////////////////// //
        // PREVIOUS MIGRATIONS SHOULD NOT BE CHANGED AS THEY HAVE BEEN DEPLOYED ON ETHERLINK //
        //                                      MAINNET                                      //
        //          REMEMBER TO MOVE THIS COMMENT ONCE NEW VERSIONS LAND ON MAINNET          //
        // ///////////////////////////////////////////////////////////////////////////////// //
        StorageVersion::V39 => {
            // Dummy migration for the blueprint generation
            Ok(MigrationStatus::Done)
        }
        StorageVersion::V40 => {
            store_evm_version(host, &EVMVersion::Osaka)?;
            Ok(MigrationStatus::Done)
        }
    }
}

// The workflow for migration is the following:
//
// - add a new variant to `storage::StorageVersion`, update `STORAGE_VERSION`
//   accordingly.
// - update `migrate_to` pattern matching  with all the needed migration functions
// - compile the kernel and run all the E2E migration tests to make sure all the
//   data is still available from the EVM proxy-node.
// - upgrade the failed_migration.wasm kernel, see tests/ressources/README.md
//
// /!\
//     If the migration takes more than 999 reboots, we will lose the inbox
//     of a level. At least one reboot must be allocated to the stage one
//     to consume the inbox. Therefore, if the migration happens to take more
//     than 999 reboots, you have to rethink this. This limitation exists
//     because we consider that the inbox should not be collected during
//     a migration because it impacts the storage. We could in theory end up
//     in an inconsistent storage.
// /!\
//
fn migration<Host: Runtime>(host: &mut Host) -> anyhow::Result<MigrationStatus> {
    match read_storage_version(host)?.next() {
        Some(next_version) => {
            let status = migrate_to(host, next_version)?;

            // Record the migration was applied. Even if the migration for `next_version` returns
            // `None`, we consider it done. A good use case for `None` is for instance for a
            // migration that does not apply to the current network.
            if status != MigrationStatus::InProgress {
                store_storage_version(host, next_version)?;
                // `InProgress` so that we reboot and try apply the next migration, if any.
                return Ok(MigrationStatus::InProgress);
            }

            Ok(status)
        }
        None => Ok(MigrationStatus::None),
    }
}

pub fn storage_migration<Host: Runtime>(
    host: &mut Host,
) -> Result<MigrationStatus, Error> {
    let migration_result = migration(host);
    migration_result.map_err(|_| Error::UpgradeError(UpgradeProcessError::Fallback))
}
