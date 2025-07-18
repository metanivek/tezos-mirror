// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use account_storage::TezlinkAccount;
use account_storage::{Manager, TezlinkImplicitAccount, TezlinkOriginatedAccount};
use mir::{
    ast::{annotations::FieldAnnotation, entrypoint, IntoMicheline, Micheline},
    context::Ctx,
    parser::Parser,
};
use num_bigint::BigInt;
use num_traits::ops::checked::CheckedSub;
use tezos_crypto_rs::{base58::FromBase58CheckError, PublicKeyWithHash};
use tezos_data_encoding::types::Narith;
use tezos_evm_logging::{log, Level::*};
use tezos_evm_runtime::runtime::Runtime;
use tezos_smart_rollup::types::{Contract, PublicKey, PublicKeyHash};
use tezos_tezlink::operation_result::{BalanceTooLow, TransferTarget, UpdateOrigin};
use tezos_tezlink::{
    operation::{
        ManagerOperation, OperationContent, Parameter, RevealContent, TransferContent,
    },
    operation_result::{
        produce_operation_result, Balance, BalanceUpdate, OperationError,
        OperationResultSum, Reveal, RevealError, RevealSuccess, TransferError,
        TransferSuccess,
    },
};
use thiserror::Error;

extern crate alloc;
pub mod account_storage;
pub mod context;
mod validate;

type ExecutionResult<A> = Result<Result<A, OperationError>, ApplyKernelError>;

#[derive(Error, Debug, PartialEq, Eq)]
pub enum ApplyKernelError {
    #[error("Apply operation failed on a storage manipulation {0}")]
    StorageError(tezos_storage::error::Error),
    #[error("Apply operation failed because of a b58 conversion {0}")]
    Base58Error(String),
    #[error("Called a non-smart contract with parameter")]
    NonSmartContractExecutionCall(),
    // TODO: #8003 propagate the error generated by MIR
    #[error("Apply operation failed because of micheline decoding {0}")]
    MichelineDecodeError(String),
    // TODO: #8003 propagate the error generated by MIR
    #[error("Failed interpreting the Michelson contract with {0}")]
    MichelsonContractInterpretError(String),
    // TODO: #8003 propagate the error generated by MIR
    #[error("Mir failed to typecheck the contract with {0}")]
    MirTypecheckingError(String),
    #[error("Apply operation failed because of a big integer conversion error {0}")]
    BigIntError(num_bigint::TryFromBigIntError<num_bigint::BigInt>),
}

// 'FromBase58CheckError' doesn't implement PartialEq and Eq
// Use the String representation instead
impl From<FromBase58CheckError> for ApplyKernelError {
    fn from(err: FromBase58CheckError) -> Self {
        Self::Base58Error(err.to_string())
    }
}

impl From<tezos_storage::error::Error> for ApplyKernelError {
    fn from(value: tezos_storage::error::Error) -> Self {
        Self::StorageError(value)
    }
}

impl From<mir::serializer::DecodeError> for ApplyKernelError {
    fn from(err: mir::serializer::DecodeError) -> Self {
        Self::MichelineDecodeError(err.to_string())
    }
}

impl From<mir::interpreter::ContractInterpretError<'_>> for ApplyKernelError {
    fn from(err: mir::interpreter::ContractInterpretError) -> Self {
        Self::MichelsonContractInterpretError(err.to_string())
    }
}

impl From<mir::typechecker::TcError> for ApplyKernelError {
    fn from(err: mir::typechecker::TcError) -> Self {
        Self::MirTypecheckingError(err.to_string())
    }
}

fn reveal<Host: Runtime>(
    host: &mut Host,
    provided_hash: &PublicKeyHash,
    account: &mut TezlinkImplicitAccount,
    public_key: &PublicKey,
) -> ExecutionResult<RevealSuccess> {
    log!(host, Debug, "Applying a reveal operation");
    let manager = account.manager(host)?;

    let expected_hash = match manager {
        Manager::Revealed(pk) => {
            return Ok(Err(RevealError::PreviouslyRevealedKey(pk).into()))
        }
        Manager::NotRevealed(pkh) => pkh,
    };

    // Ensure that the source of the operation is equal to the retrieved hash.
    if &expected_hash != provided_hash {
        return Ok(Err(RevealError::InconsistentHash(expected_hash).into()));
    }

    // Check the public key
    let pkh_from_pk = public_key.pk_hash();
    if expected_hash != pkh_from_pk {
        return Ok(Err(RevealError::InconsistentPublicKey(expected_hash).into()));
    }

    // Set the public key as the manager
    account.set_manager_public_key(host, public_key)?;
    account.increment_counter(host)?;

    log!(host, Debug, "Reveal operation succeed");

    Ok(Ok(RevealSuccess {
        consumed_gas: 0_u64.into(),
    }))
}

/// Handles manager transfer operations for both implicit and originated contracts.
pub fn transfer<Host: Runtime>(
    host: &mut Host,
    context: &context::Context,
    src: &PublicKeyHash,
    amount: &Narith,
    dest: &Contract,
    parameter: &Option<Parameter>,
) -> ExecutionResult<TransferTarget> {
    log!(
        host,
        Debug,
        "Applying a transfer operation from {} to {:?} of {:?} mutez",
        src,
        dest,
        amount
    );

    let src_contract = Contract::Implicit(src.clone());
    let (src_update, dest_update) = compute_balance_updates(&src_contract, dest, amount)
        .map_err(ApplyKernelError::BigIntError)?;

    // Check source balance
    let mut src_account = TezlinkImplicitAccount::from_public_key_hash(context, src)?;
    let current_src_balance = src_account.balance(host)?.0;

    let new_source_balance = match current_src_balance.checked_sub(&amount.0) {
        None => {
            log!(host, Debug, "Balance is too low");
            let error = TransferError::BalanceTooLow(BalanceTooLow {
                contract: src_contract.clone(),
                balance: current_src_balance.into(),
                amount: amount.clone(),
            });
            return Ok(Err(error.into()));
        }
        Some(new_source_balance) => new_source_balance,
    };

    // Delegate to appropriate handler
    let success = match dest {
        Contract::Implicit(dest_key_hash) => {
            if parameter.is_some() {
                return Err(ApplyKernelError::NonSmartContractExecutionCall());
            }
            let allocated = TezlinkImplicitAccount::allocate(host, context, dest)?;
            let mut dest_account =
                TezlinkImplicitAccount::from_public_key_hash(context, dest_key_hash)?;
            apply_balance_changes(
                host,
                &mut src_account,
                new_source_balance.clone(),
                &mut dest_account,
                &amount.0,
            )?;

            TransferTarget::ToContrat(TransferSuccess {
                storage: None,
                lazy_storage_diff: None,
                balance_updates: vec![src_update, dest_update],
                ticket_receipt: vec![],
                originated_contracts: vec![],
                consumed_gas: 0_u64.into(),
                storage_size: 0_u64.into(),
                paid_storage_size_diff: 0_u64.into(),
                allocated_destination_contract: allocated,
            })
        }

        Contract::Originated(_) => {
            let mut dest_contract =
                TezlinkOriginatedAccount::from_contract(context, dest)?;
            apply_balance_changes(
                host,
                &mut src_account,
                new_source_balance.clone(),
                &mut dest_contract,
                &amount.0,
            )?;

            let new_storage =
                execute_smart_contract(host, &mut dest_contract, parameter)?;

            TransferTarget::ToContrat(TransferSuccess {
                storage: Some(new_storage),
                lazy_storage_diff: None,
                balance_updates: vec![src_update, dest_update],
                ticket_receipt: vec![],
                originated_contracts: vec![],
                consumed_gas: 0_u64.into(),
                storage_size: 0_u64.into(),
                paid_storage_size_diff: 0_u64.into(),
                allocated_destination_contract: false,
            })
        }
    };

    src_account.increment_counter(host)?;
    Ok(Ok(success))
}

/// Prepares balance updates when accounting fees in the format expected by the Tezos operation.
fn compute_fees_balance_updates(
    source: &PublicKeyHash,
    amount: &Narith,
) -> Result<
    (BalanceUpdate, BalanceUpdate),
    num_bigint::TryFromBigIntError<num_bigint::BigInt>,
> {
    let source_delta = BigInt::from_biguint(num_bigint::Sign::Minus, amount.into());
    let block_fees = BigInt::from_biguint(num_bigint::Sign::Plus, amount.into());

    let src_update = BalanceUpdate {
        balance: Balance::Account(Contract::Implicit(source.clone())),
        changes: source_delta.try_into()?,
        update_origin: UpdateOrigin::BlockApplication,
    };

    let block_fees = BalanceUpdate {
        balance: Balance::BlockFees,
        changes: block_fees.try_into()?,
        update_origin: UpdateOrigin::BlockApplication,
    };

    Ok((src_update, block_fees))
}

/// Prepares balance updates in the format expected by the Tezos operation.
fn compute_balance_updates(
    src: &Contract,
    dest: &Contract,
    amount: &Narith,
) -> Result<
    (BalanceUpdate, BalanceUpdate),
    num_bigint::TryFromBigIntError<num_bigint::BigInt>,
> {
    let src_delta = BigInt::from_biguint(num_bigint::Sign::Minus, amount.into());
    let dest_delta = BigInt::from_biguint(num_bigint::Sign::Plus, amount.into());

    let src_update = BalanceUpdate {
        balance: Balance::Account(src.clone()),
        changes: src_delta.try_into()?,
        update_origin: UpdateOrigin::BlockApplication,
    };

    let dest_update = BalanceUpdate {
        balance: Balance::Account(dest.clone()),
        changes: dest_delta.try_into()?,
        update_origin: UpdateOrigin::BlockApplication,
    };

    Ok((src_update, dest_update))
}

/// Applies balance changes by updating both source and destination accounts.
fn apply_balance_changes(
    host: &mut impl Runtime,
    src_account: &mut impl TezlinkAccount,
    new_src_balance: num_bigint::BigUint,
    dest_account: &mut impl TezlinkAccount,
    amount: &num_bigint::BigUint,
) -> Result<(), ApplyKernelError> {
    src_account.set_balance(host, &new_src_balance.into())?;
    let dest_balance = dest_account.balance(host)?.0;
    let new_dest_balance = &dest_balance + amount;
    dest_account.set_balance(host, &new_dest_balance.into())?;
    Ok(())
}

/// Executes the entrypoint logic of an originated smart contract and returns the new storage and consumed gas.
fn execute_smart_contract<Host: Runtime>(
    host: &mut Host,
    destination: &mut TezlinkOriginatedAccount,
    parameter: &Option<Parameter>,
) -> Result<Vec<u8>, ApplyKernelError> {
    let parser = Parser::new();
    let code = destination.code(host)?;
    let storage = destination.storage(host)?;
    let contract_micheline = Micheline::decode_raw(&parser.arena, &code)?;

    let (entrypoint, value) = match parameter {
        Some(param) => (
            param.entrypoint.clone(),
            Micheline::decode_raw(&parser.arena, &param.value)?,
        ),
        None => (entrypoint::Entrypoint::default(), Micheline::from(())),
    };

    let mut ctx = Ctx::default();
    let contract_typechecked = contract_micheline.typecheck_script(&mut ctx)?;

    let storage = Micheline::decode_raw(&parser.arena, &storage)?;

    let (_, new_storage) = contract_typechecked.interpret(
        &mut ctx,
        &parser.arena,
        value,
        Some(FieldAnnotation::from_str_unchecked(entrypoint.as_str())),
        storage,
    )?;

    let new_storage = new_storage
        .into_micheline_optimized_legacy(&parser.arena)
        .encode();
    let _ = destination.set_storage(host, &new_storage);
    Ok(new_storage)
}

pub fn apply_operation<Host: Runtime>(
    host: &mut Host,
    context: &context::Context,
    operation: ManagerOperation<OperationContent>,
) -> Result<OperationResultSum, ApplyKernelError> {
    let source = &operation.source;
    log!(
        host,
        Debug,
        "Going to run a Tezos Manager Operation from {}",
        source
    );

    let mut account = TezlinkImplicitAccount::from_public_key_hash(context, source)?;

    log!(host, Debug, "Verifying that the operation is valid");

    let validity_result =
        validate::is_valid_tezlink_operation(host, &account, &operation)?;

    let new_balance = match validity_result {
        Ok(new_balance) => new_balance,
        Err(validity_err) => {
            log!(host, Debug, "Operation is invalid, exiting apply_operation");
            // TODO: Don't force the receipt to a reveal receipt
            let receipt = produce_operation_result::<Reveal>(
                vec![],
                Err(OperationError::Validation(validity_err)),
            );
            return Ok(OperationResultSum::Reveal(receipt));
        }
    };

    log!(host, Debug, "Operation is valid");

    log!(host, Debug, "Updates balance to pay fees");
    account.set_balance(host, &new_balance)?;

    let (src_delta, block_fees) = compute_fees_balance_updates(source, &operation.fee)
        .map_err(ApplyKernelError::BigIntError)?;

    let receipt = match operation.operation {
        OperationContent::Reveal(RevealContent { pk }) => {
            let reveal_result = reveal(host, source, &mut account, &pk)?;
            let manager_result =
                produce_operation_result(vec![src_delta, block_fees], reveal_result);
            OperationResultSum::Reveal(manager_result)
        }
        OperationContent::Transfer(TransferContent {
            amount,
            destination,
            parameters,
        }) => {
            let transfer_result =
                transfer(host, context, source, &amount, &destination, &parameters)?;
            let manager_result =
                produce_operation_result(vec![src_delta, block_fees], transfer_result);
            OperationResultSum::Transfer(manager_result)
        }
    };

    Ok(receipt)
}

#[cfg(test)]
mod tests {
    use crate::TezlinkImplicitAccount;
    use tezos_crypto_rs::hash::UnknownSignature;
    use tezos_data_encoding::types::Narith;
    use tezos_evm_runtime::runtime::{MockKernelHost, Runtime};
    use tezos_smart_rollup::types::{Contract, PublicKey, PublicKeyHash};
    use tezos_tezlink::{
        block::TezBlock,
        enc_wrappers::BlockHash,
        operation::{
            ManagerOperation, Operation, OperationContent, Parameter, RevealContent,
            TransferContent,
        },
        operation_result::{
            Balance, BalanceTooLow, BalanceUpdate, ContentResult, CounterError,
            OperationResult, OperationResultSum, RevealError, RevealSuccess,
            TransferError, TransferSuccess, TransferTarget, UpdateOrigin, ValidityError,
        },
    };

    use crate::{
        account_storage::{Manager, TezlinkAccount},
        apply_operation, context, OperationError,
    };

    const BOOTSTRAP_1: &str = "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx";

    const BOOTSTRAP_2: &str = "tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN";

    fn make_operation(
        fee: u64,
        counter: u64,
        gas_limit: u64,
        storage_limit: u64,
        source: PublicKeyHash,
        content: OperationContent,
    ) -> Operation {
        let branch = BlockHash::from(TezBlock::genesis_block_hash());
        // No need a real signature for now
        let signature = UnknownSignature::from_base58_check("sigSPESPpW4p44JK181SmFCFgZLVvau7wsJVN85bv5ciigMu7WSRnxs9H2NydN5ecxKHJBQTudFPrUccktoi29zHYsuzpzBX").unwrap();
        Operation {
            branch,
            content: ManagerOperation {
                source,
                fee: fee.into(),
                counter: counter.into(),
                operation: content,
                gas_limit: gas_limit.into(),
                storage_limit: storage_limit.into(),
            }
            .into(),
            signature,
        }
    }

    fn make_reveal_operation(
        fee: u64,
        counter: u64,
        gas_limit: u64,
        storage_limit: u64,
        source: PublicKeyHash,
        pk: PublicKey,
    ) -> Operation {
        make_operation(
            fee,
            counter,
            gas_limit,
            storage_limit,
            source,
            OperationContent::Reveal(RevealContent { pk }),
        )
    }

    #[allow(clippy::too_many_arguments)]
    fn make_transfer_operation(
        fee: u64,
        counter: u64,
        gas_limit: u64,
        storage_limit: u64,
        source: PublicKeyHash,
        amount: Narith,
        destination: Contract,
        parameters: Option<Parameter>,
    ) -> Operation {
        make_operation(
            fee,
            counter,
            gas_limit,
            storage_limit,
            source,
            OperationContent::Transfer(TransferContent {
                amount,
                destination,
                parameters,
            }),
        )
    }

    // This function setups an account that will pass the validity checks
    fn init_account(
        host: &mut impl Runtime,
        src: &PublicKeyHash,
    ) -> TezlinkImplicitAccount {
        // Setting the account in TezlinkImplicitAccount
        let contract = Contract::from_b58check(&src.to_b58check())
            .expect("Contract b58 conversion should have succeed");

        let context = context::Context::init_context();

        // Allocate the account
        TezlinkImplicitAccount::allocate(host, &context, &contract)
            .expect("Account initialization should have succeed");

        let mut account = TezlinkImplicitAccount::from_contract(&context, &contract)
            .expect("Account creation should have succeed");

        // Setting the balance to pass the validity check
        account
            .set_balance(host, &50_u64.into())
            .expect("Set balance should have succeed");

        account
    }

    // Test an operation on an account that has no entry in `/context/contracts/index`
    // This should fail as an EmptyImplicitContract
    #[test]
    fn apply_operation_empty_account() {
        let mut host = MockKernelHost::default();

        let src = PublicKeyHash::from_b58check(BOOTSTRAP_1)
            .expect("PublicKeyHash b58 conversion should have succeed");

        let pk = PublicKey::from_b58check(
            "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav",
        )
        .expect("Public key creation should have succeed");

        let operation = make_reveal_operation(15, 1, 4, 5, src, pk);

        let receipt = apply_operation(
            &mut host,
            &context::Context::init_context(),
            operation.content.into(),
        )
        .expect("apply_operation should not have failed with a kernel error");

        let expected_receipt = OperationResultSum::Reveal(OperationResult {
            balance_updates: vec![],
            result: ContentResult::Failed(vec![OperationError::Validation(
                ValidityError::EmptyImplicitContract,
            )]),
            internal_operation_results: vec![],
        });

        assert_eq!(receipt, expected_receipt);
    }

    // Test that increasing the fees makes the operation fails
    #[test]
    fn apply_operation_cant_pay_fees() {
        let mut host = MockKernelHost::default();

        let src = PublicKeyHash::from_b58check(BOOTSTRAP_1)
            .expect("PublicKeyHash b58 conversion should have succeed");

        let _ = init_account(&mut host, &src);

        let pk = PublicKey::from_b58check(
            "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav",
        )
        .expect("Public key creation should have succeed");

        // Fees are too high for source's balance
        let operation = make_reveal_operation(100, 1, 4, 5, src, pk);

        let receipt = apply_operation(
            &mut host,
            &context::Context::init_context(),
            operation.content.into(),
        )
        .expect("apply_operation should not have failed with a kernel error");

        let expected_receipt = OperationResultSum::Reveal(OperationResult {
            balance_updates: vec![],
            result: ContentResult::Failed(vec![OperationError::Validation(
                ValidityError::CantPayFees(100_u64.into()),
            )]),
            internal_operation_results: vec![],
        });

        assert_eq!(receipt, expected_receipt);
    }

    // Test that a wrong counter should make the operation fails
    #[test]
    fn apply_operation_invalid_counter() {
        let mut host = MockKernelHost::default();

        let src = PublicKeyHash::from_b58check(BOOTSTRAP_1)
            .expect("PublicKeyHash b58 conversion should have succeed");

        let _ = init_account(&mut host, &src);

        let pk = PublicKey::from_b58check(
            "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav",
        )
        .expect("Public key creation should have succeed");

        // Counter is incoherent for source's counter
        let operation = make_reveal_operation(15, 15, 4, 5, src, pk);

        let receipt = apply_operation(
            &mut host,
            &context::Context::init_context(),
            operation.content.into(),
        )
        .expect("apply_operation should not have failed with a kernel error");

        let expected_receipt = OperationResultSum::Reveal(OperationResult {
            balance_updates: vec![],
            result: ContentResult::Failed(vec![OperationError::Validation(
                ValidityError::CounterInTheFuture(CounterError {
                    expected: 1_u64.into(),
                    found: 15_u64.into(),
                }),
            )]),
            internal_operation_results: vec![],
        });
        assert_eq!(receipt, expected_receipt);
    }

    // At this point, tests are focused on the content of the operation. We should not revert with ValidityError anymore.
    // Test a reveal operation on an already revealed account
    #[test]
    fn apply_reveal_operation_on_already_revealed_account() {
        let mut host = MockKernelHost::default();

        let src = PublicKeyHash::from_b58check(BOOTSTRAP_1)
            .expect("PublicKeyHash b58 conversion should have succeed");

        let mut account = init_account(&mut host, &src);

        // Setting the manager key of this account to its public_key, this account
        // will be considered as revealed and the reveal operation should fail
        let pk = PublicKey::from_b58check(
            "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav",
        )
        .expect("Public key creation should have succeed");

        account
            .set_manager_public_key(&mut host, &pk)
            .expect("Setting manager field should have succeed");

        // Applying the operation
        let operation = make_reveal_operation(15, 1, 4, 5, src.clone(), pk.clone());
        let receipt = apply_operation(
            &mut host,
            &context::Context::init_context(),
            operation.content.into(),
        )
        .expect("apply_operation should not have failed with a kernel error");

        // Reveal operation should fail
        let expected_receipt = OperationResultSum::Reveal(OperationResult {
            balance_updates: vec![
                BalanceUpdate {
                    balance: Balance::Account(Contract::Implicit(src)),
                    changes: -15,
                    update_origin: UpdateOrigin::BlockApplication,
                },
                BalanceUpdate {
                    balance: Balance::BlockFees,
                    changes: 15,
                    update_origin: UpdateOrigin::BlockApplication,
                },
            ],
            result: ContentResult::Failed(vec![OperationError::Apply(
                RevealError::PreviouslyRevealedKey(pk).into(),
            )]),
            internal_operation_results: vec![],
        });
        assert_eq!(receipt, expected_receipt);
    }

    // Test an invalid reveal operation where the manager is inconsistent for source
    // (where source is different of the manager field)
    #[test]
    fn apply_reveal_operation_with_an_inconsistent_manager() {
        let mut host = MockKernelHost::default();

        let src = PublicKeyHash::from_b58check(BOOTSTRAP_1)
            .expect("PublicKeyHash b58 conversion should have succeed");

        let mut account = init_account(&mut host, &src);

        // Set the an inconsistent manager with the source
        let inconsistent_pkh =
            PublicKeyHash::from_b58check("tz1UEQcU7M43yUECMpKGJcxCVwHRaP819qhN")
                .expect("PublicKeyHash b58 conversion should have succeed");

        account
            .set_manager_public_key_hash(&mut host, &inconsistent_pkh)
            .expect("Setting manager field should have succeed");

        let pk = PublicKey::from_b58check(
            "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav",
        )
        .expect("Public key creation should have succeed");

        let operation = make_reveal_operation(15, 1, 4, 5, src.clone(), pk);

        let receipt = apply_operation(
            &mut host,
            &context::Context::init_context(),
            operation.content.into(),
        )
        .expect("apply_operation should not have failed with a kernel error");

        let expected_receipt = OperationResultSum::Reveal(OperationResult {
            balance_updates: vec![
                BalanceUpdate {
                    balance: Balance::Account(Contract::Implicit(src)),
                    changes: -15,
                    update_origin: UpdateOrigin::BlockApplication,
                },
                BalanceUpdate {
                    balance: Balance::BlockFees,
                    changes: 15,
                    update_origin: UpdateOrigin::BlockApplication,
                },
            ],
            result: ContentResult::Failed(vec![OperationError::Apply(
                RevealError::InconsistentHash(inconsistent_pkh).into(),
            )]),
            internal_operation_results: vec![],
        });

        assert_eq!(receipt, expected_receipt);
    }

    // Test an invalid operation where the provided public key is inconsistent for the source
    #[test]
    fn apply_reveal_operation_with_an_inconsistent_public_key() {
        let mut host = MockKernelHost::default();

        let src = PublicKeyHash::from_b58check(BOOTSTRAP_1)
            .expect("PublicKeyHash b58 conversion should have succeed");

        // Even if we don't use it we need to init the account
        let _ = init_account(&mut host, &src);

        // Wrong public key for source
        let pk = PublicKey::from_b58check(
            "edpkuT1qccDweCHnvgjLuNUHERpZmEaFZfbWvTzj2BxmTgQBZjaDFD",
        )
        .expect("Public key creation should have succeed");

        let operation = make_reveal_operation(15, 1, 4, 5, src.clone(), pk);

        let receipt = apply_operation(
            &mut host,
            &context::Context::init_context(),
            operation.content.into(),
        )
        .expect("apply_operation should not have failed with a kernel error");

        let expected_receipt = OperationResultSum::Reveal(OperationResult {
            balance_updates: vec![
                BalanceUpdate {
                    balance: Balance::Account(Contract::Implicit(src.clone())),
                    changes: -15,
                    update_origin: UpdateOrigin::BlockApplication,
                },
                BalanceUpdate {
                    balance: Balance::BlockFees,
                    changes: 15,
                    update_origin: UpdateOrigin::BlockApplication,
                },
            ],
            result: ContentResult::Failed(vec![OperationError::Apply(
                RevealError::InconsistentPublicKey(src).into(),
            )]),
            internal_operation_results: vec![],
        });

        assert_eq!(receipt, expected_receipt);
    }

    // Test a valid reveal operation, the manager should go from NotRevealed to Revealed
    #[test]
    fn apply_reveal_operation() {
        let mut host = MockKernelHost::default();

        let src = PublicKeyHash::from_b58check(BOOTSTRAP_1)
            .expect("PublicKeyHash b58 conversion should have succeed");

        let account = init_account(&mut host, &src);

        let manager = account
            .manager(&host)
            .expect("Read manager should have succeed");

        assert_eq!(manager, Manager::NotRevealed(src.clone()));

        let pk = PublicKey::from_b58check(
            "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav",
        )
        .expect("Public key creation should have succeed");

        let operation = make_reveal_operation(15, 1, 4, 5, src.clone(), pk.clone());

        let receipt = apply_operation(
            &mut host,
            &context::Context::init_context(),
            operation.content.into(),
        )
        .expect("apply_operation should not have failed with a kernel error");

        let expected_receipt = OperationResultSum::Reveal(OperationResult {
            balance_updates: vec![
                BalanceUpdate {
                    balance: Balance::Account(Contract::Implicit(src)),
                    changes: -15,
                    update_origin: UpdateOrigin::BlockApplication,
                },
                BalanceUpdate {
                    balance: Balance::BlockFees,
                    changes: 15,
                    update_origin: UpdateOrigin::BlockApplication,
                },
            ],
            result: ContentResult::Applied(RevealSuccess {
                consumed_gas: 0_u64.into(),
            }),
            internal_operation_results: vec![],
        });

        assert_eq!(receipt, expected_receipt);

        let manager = account
            .manager(&host)
            .expect("Read manager should have succeed");

        assert_eq!(manager, Manager::Revealed(pk));
    }

    // Test an invalid transfer operation, source has not enough balance to fullfil the Transfer
    #[test]
    fn apply_transfer_with_not_enough_balance() {
        let mut host = MockKernelHost::default();

        let src = PublicKeyHash::from_b58check(BOOTSTRAP_1)
            .expect("PublicKeyHash b58 conversion should have succeed");

        let dest = PublicKeyHash::from_b58check(BOOTSTRAP_2)
            .expect("PublicKeyHash b58 conversion should have succeed");

        // Setup accounts with 50 mutez in their balance
        let source = init_account(&mut host, &src);
        let destination = init_account(&mut host, &dest);

        let operation = make_transfer_operation(
            15,
            1,
            4,
            5,
            src.clone(),
            100_u64.into(),
            Contract::Implicit(dest),
            None,
        );

        let receipt = apply_operation(
            &mut host,
            &context::Context::init_context(),
            operation.content.into(),
        )
        .expect("apply_operation should not have failed with a kernel error");

        let expected_receipt = OperationResultSum::Transfer(OperationResult {
            balance_updates: vec![
                BalanceUpdate {
                    balance: Balance::Account(Contract::Implicit(src)),
                    changes: -15,
                    update_origin: UpdateOrigin::BlockApplication,
                },
                BalanceUpdate {
                    balance: Balance::BlockFees,
                    changes: 15,
                    update_origin: UpdateOrigin::BlockApplication,
                },
            ],
            result: ContentResult::Failed(vec![OperationError::Apply(
                TransferError::BalanceTooLow(BalanceTooLow {
                    contract: Contract::from_b58check(BOOTSTRAP_1).unwrap(),
                    balance: 35_u64.into(),
                    amount: 100_u64.into(),
                })
                .into(),
            )]),
            internal_operation_results: vec![],
        });

        // Verify that source only paid the fees and the destination balance is unchanged
        assert_eq!(source.balance(&host).unwrap(), 35.into());
        assert_eq!(destination.balance(&host).unwrap(), 50_u64.into());

        assert_eq!(receipt, expected_receipt);
    }

    // Bootstrap 1 successfully transfer 30 mutez to Bootstrap 2
    #[test]
    fn apply_successful_transfer() {
        let mut host = MockKernelHost::default();

        let src = PublicKeyHash::from_b58check(BOOTSTRAP_1)
            .expect("PublicKeyHash b58 conversion should have succeed");

        let dest = PublicKeyHash::from_b58check(BOOTSTRAP_2)
            .expect("PublicKeyHash b58 conversion should have succeed");

        // Setup accounts with 50 mutez in their balance
        let source = init_account(&mut host, &src);
        let destination = init_account(&mut host, &dest);

        let operation = make_transfer_operation(
            15,
            1,
            4,
            5,
            src.clone(),
            30_u64.into(),
            Contract::Implicit(dest),
            None,
        );

        let receipt = apply_operation(
            &mut host,
            &context::Context::init_context(),
            operation.content.into(),
        )
        .expect("apply_operation should not have failed with a kernel error");

        let expected_receipt = OperationResultSum::Transfer(OperationResult {
            balance_updates: vec![
                BalanceUpdate {
                    balance: Balance::Account(Contract::Implicit(src)),
                    changes: -15,
                    update_origin: UpdateOrigin::BlockApplication,
                },
                BalanceUpdate {
                    balance: Balance::BlockFees,
                    changes: 15,
                    update_origin: UpdateOrigin::BlockApplication,
                },
            ],
            result: ContentResult::Applied(TransferTarget::ToContrat(TransferSuccess {
                storage: None,
                lazy_storage_diff: None,
                balance_updates: vec![
                    BalanceUpdate {
                        balance: Balance::Account(
                            Contract::from_b58check(BOOTSTRAP_1).unwrap(),
                        ),
                        changes: -30,
                        update_origin: UpdateOrigin::BlockApplication,
                    },
                    BalanceUpdate {
                        balance: Balance::Account(
                            Contract::from_b58check(BOOTSTRAP_2).unwrap(),
                        ),
                        changes: 30,
                        update_origin: UpdateOrigin::BlockApplication,
                    },
                ],
                ticket_receipt: vec![],
                originated_contracts: vec![],
                consumed_gas: 0_u64.into(),
                storage_size: 0_u64.into(),
                paid_storage_size_diff: 0_u64.into(),
                allocated_destination_contract: true,
            })),
            internal_operation_results: vec![],
        });

        // Verify that source and destination balances changed
        assert_eq!(source.balance(&host).unwrap(), 5_u64.into());
        assert_eq!(destination.balance(&host).unwrap(), 80_u64.into());

        assert_eq!(receipt, expected_receipt);
    }

    // Bootstrap 1 successfully transfers 30 mutez to itself
    #[test]
    fn apply_successful_self_transfer() {
        let mut host = MockKernelHost::default();

        let src = PublicKeyHash::from_b58check(BOOTSTRAP_1)
            .expect("PublicKeyHash b58 conversion should have succeeded");

        let dest = src.clone();

        // Setup account with 50 mutez in its balance
        let source = init_account(&mut host, &src);

        let operation = make_transfer_operation(
            15,
            1,
            4,
            5,
            src,
            30_u64.into(),
            Contract::Implicit(dest),
            None,
        );

        let receipt = apply_operation(
            &mut host,
            &context::Context::init_context(),
            operation.content.into(),
        )
        .expect("apply_operation should not have failed with a kernel error");

        let expected_receipt = OperationResultSum::Transfer(OperationResult {
            balance_updates: vec![
                BalanceUpdate {
                    balance: Balance::Account(
                        Contract::from_b58check(BOOTSTRAP_1).unwrap(),
                    ),
                    changes: -15,
                    update_origin: UpdateOrigin::BlockApplication,
                },
                BalanceUpdate {
                    balance: Balance::BlockFees,
                    changes: 15,
                    update_origin: UpdateOrigin::BlockApplication,
                },
            ],
            result: ContentResult::Applied(TransferTarget::ToContrat(TransferSuccess {
                storage: None,
                lazy_storage_diff: None,
                balance_updates: vec![
                    BalanceUpdate {
                        balance: Balance::Account(
                            Contract::from_b58check(BOOTSTRAP_1).unwrap(),
                        ),
                        changes: -30,
                        update_origin: UpdateOrigin::BlockApplication,
                    },
                    BalanceUpdate {
                        balance: Balance::Account(
                            Contract::from_b58check(BOOTSTRAP_1).unwrap(),
                        ),
                        changes: 30,
                        update_origin: UpdateOrigin::BlockApplication,
                    },
                ],
                ticket_receipt: vec![],
                originated_contracts: vec![],
                consumed_gas: 0_u64.into(),
                storage_size: 0_u64.into(),
                paid_storage_size_diff: 0_u64.into(),
                allocated_destination_contract: true,
            })),
            internal_operation_results: vec![],
        });

        // Verify that balance was only debited for fees
        assert_eq!(source.balance(&host).unwrap(), 35_u64.into());

        assert_eq!(receipt, expected_receipt);
    }
}
