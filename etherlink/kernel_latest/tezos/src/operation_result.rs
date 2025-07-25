// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

//! Tezos operations

/// The whole module is inspired of `src/proto_alpha/lib_protocol/apply_result.ml` to represent the result of an operation
/// In Tezlink, operation is equivalent to manager operation because there is no other type of operation that interests us.
use std::fmt::Debug;
use tezos_crypto_rs::hash::UnknownSignature;
use tezos_data_encoding::enc as tezos_enc;
use tezos_data_encoding::nom as tezos_nom;
use tezos_data_encoding::types::Narith;
use tezos_enc::BinWriter;
use tezos_nom::NomReader;
use tezos_smart_rollup::types::Contract;
use tezos_smart_rollup::types::{PublicKey, PublicKeyHash};

use crate::operation::ManagerOperationContent;

#[derive(Debug, PartialEq, Eq, NomReader, BinWriter)]
pub struct CounterError {
    pub expected: Narith,
    pub found: Narith,
}

#[derive(Debug, PartialEq, Eq, NomReader, BinWriter)]
pub enum ValidityError {
    CounterInThePast(CounterError),
    CounterInTheFuture(CounterError),
    MissingManagerContract,
    UnrevealedManagerKey(PublicKeyHash),
    CantPayFees(Narith),
    EmptyImplicitContract,
    GasLimitTooHigh,
    StorageLimitTooHigh,
    InvalidSignature,
}

#[derive(Debug, PartialEq, Eq, NomReader, BinWriter)]
pub enum RevealError {
    PreviouslyRevealedKey(PublicKey),
    InconsistentHash(PublicKeyHash),
    InconsistentPublicKey(PublicKeyHash),
}

#[derive(Debug, PartialEq, Eq, BinWriter, NomReader)]
pub struct BalanceTooLow {
    pub contract: Contract,
    pub balance: Narith,
    pub amount: Narith,
}

#[derive(Debug, PartialEq, Eq, BinWriter, NomReader)]
pub enum TransferError {
    BalanceTooLow(BalanceTooLow),
    UnspendableContract(Contract),
}

#[derive(Debug, PartialEq, Eq, NomReader, BinWriter)]
pub enum ApplyOperationError {
    Reveal(RevealError),
    Transfer(TransferError),
}

impl From<RevealError> for ApplyOperationError {
    fn from(value: RevealError) -> Self {
        Self::Reveal(value)
    }
}

impl From<TransferError> for ApplyOperationError {
    fn from(value: TransferError) -> Self {
        Self::Transfer(value)
    }
}

impl From<RevealError> for OperationError {
    fn from(value: RevealError) -> Self {
        Self::Apply(value.into())
    }
}

impl From<TransferError> for OperationError {
    fn from(value: TransferError) -> Self {
        Self::Apply(value.into())
    }
}

#[derive(Debug, PartialEq, Eq, NomReader, BinWriter)]
pub enum OperationError {
    Validation(ValidityError),
    Apply(ApplyOperationError),
}

impl From<ValidityError> for OperationError {
    fn from(value: ValidityError) -> Self {
        Self::Validation(value)
    }
}

impl From<ApplyOperationError> for OperationError {
    fn from(value: ApplyOperationError) -> Self {
        Self::Apply(value)
    }
}

pub trait OperationKind {
    type Success: PartialEq + Debug + BinWriter + for<'a> NomReader<'a>;
}

/// Empty struct to implement [OperationKind] trait for Reveal
#[derive(PartialEq, Debug)]
pub struct Reveal;

/// Empty struct to implement [OperationKind] trait for Transfer
#[derive(PartialEq, Debug)]
pub struct Transfer;

impl OperationKind for Transfer {
    type Success = TransferTarget;
}

impl OperationKind for Reveal {
    type Success = RevealSuccess;
}

// Inspired from `src/proto_alpha/lib_protocol/apply_results.ml` : transaction_contract_variant_cases
#[derive(PartialEq, Debug, NomReader, BinWriter)]
pub enum TransferTarget {
    ToContrat(TransferSuccess),
}

#[derive(PartialEq, Debug, NomReader, BinWriter)]
pub struct RevealSuccess {
    pub consumed_gas: Narith,
}

// PlaceHolder Type for temporary unused fields
#[derive(PartialEq, Debug, Clone)]
pub struct Empty;

impl BinWriter for Empty {
    fn bin_write(&self, _: &mut Vec<u8>) -> tezos_enc::BinResult {
        Ok(())
    }
}

impl NomReader<'_> for Empty {
    fn nom_read(input: &'_ [u8]) -> tezos_nom::NomResult<'_, Self> {
        Ok((input, Self))
    }
}

#[derive(PartialEq, Debug, BinWriter, NomReader)]
pub struct TransferSuccess {
    pub storage: Option<Vec<u8>>,
    #[encoding(dynamic, list)]
    pub balance_updates: Vec<BalanceUpdate>,
    // TODO: Placeholder for ticket receipt issue : #8018
    #[encoding(dynamic, bytes)]
    pub ticket_receipt: Vec<u8>,
    // TODO: Placeholder for originated contracts issue : #8018
    #[encoding(dynamic, bytes)]
    pub originated_contracts: Vec<u8>,
    pub consumed_gas: Narith,
    pub storage_size: Narith,
    pub paid_storage_size_diff: Narith,
    pub allocated_destination_contract: bool,
    // TODO: Placeholder for lazy storage diff issue : #8018
    pub lazy_storage_diff: Option<()>,
}

// An operation error in a Tezos receipt has no specific format
// It should just be encoded as a JSON, so we can't derive
// NomReader and BinWriter if we want to be Tezos compatible
#[derive(PartialEq, Debug)]
pub struct OperationErrors {
    pub errors: Vec<OperationError>,
}

impl From<Vec<OperationError>> for OperationErrors {
    fn from(value: Vec<OperationError>) -> Self {
        OperationErrors { errors: value }
    }
}

impl BinWriter for OperationErrors {
    fn bin_write(&self, output: &mut Vec<u8>) -> tezos_enc::BinResult {
        tezos_enc::dynamic(tezos_enc::unit)(&(), output)
    }
}

impl NomReader<'_> for OperationErrors {
    fn nom_read(input: &'_ [u8]) -> tezos_nom::NomResult<'_, Self> {
        tezos_nom::dynamic(|input| Ok((input, vec![].into())))(input)
    }
}

// Inspired from `operation_result` in `src/proto_alpha/lib_protocol/apply_operation_result.ml`
// Still need to implement Backtracked and Skipped
#[derive(PartialEq, Debug, BinWriter, NomReader)]
pub enum ContentResult<M: OperationKind> {
    Applied(M::Success),
    Failed(OperationErrors),
}

/// A [Balance] updates can be triggered on different target
/// inspired from src/proto_alpha/lib_protocol/receipt_repr.ml
#[derive(PartialEq, Debug, NomReader, BinWriter)]
#[encoding(tags = "u8")]
pub enum Balance {
    #[encoding(tag = 0)]
    Account(Contract),
    // Don't know why but in balance_and_update in receipt_repr.ml,
    // the tag 1 doesn't exist
    #[encoding(tag = 2)]
    BlockFees,
}

/// Inspired from update_origin_encoding src/proto_alpha/lib_protocol/receipt_repr.ml
#[derive(PartialEq, Debug, NomReader, BinWriter)]
pub enum UpdateOrigin {
    BlockApplication,
}

/// Depending of the sign of [changes], the balance is credited or debited
/// Inspired from balance_updates_encoding src/proto_alpha/lib_protocol/receipt_repr.ml
#[derive(PartialEq, Debug, NomReader, BinWriter)]
pub struct BalanceUpdate {
    pub balance: Balance,
    pub changes: i64,
    pub update_origin: UpdateOrigin,
}

// Inspired from `Manager_operation_result` case in 'kind contents_result type
// from `src/proto_alpha/lib_protocol/apply_results.ml` file.
// Still need to implement internal_results
#[derive(PartialEq, Debug, NomReader, BinWriter)]
pub struct OperationResult<M: OperationKind> {
    #[encoding(dynamic, list)]
    pub balance_updates: Vec<BalanceUpdate>,
    pub result: ContentResult<M>,
    //TODO Placeholder for internal operations : #8018
    #[encoding(dynamic, bytes)]
    pub internal_operation_results: Vec<u8>,
}
#[derive(PartialEq, Debug)]
pub enum OperationResultSum {
    Reveal(OperationResult<Reveal>),
    Transfer(OperationResult<Transfer>),
}

pub fn is_applied(res: &OperationResultSum) -> bool {
    match res {
        OperationResultSum::Reveal(op_res) => {
            matches!(op_res.result, ContentResult::Applied(_))
        }
        OperationResultSum::Transfer(op_res) => {
            matches!(op_res.result, ContentResult::Applied(_))
        }
    }
}

pub fn produce_operation_result<M: OperationKind>(
    balance_updates: Vec<BalanceUpdate>,
    result: Result<M::Success, OperationError>,
) -> OperationResult<M> {
    match result {
        Ok(success) => OperationResult {
            balance_updates,
            result: ContentResult::Applied(success),
            internal_operation_results: vec![],
        },
        Err(operation_error) => OperationResult {
            balance_updates,
            result: ContentResult::Failed(vec![operation_error].into()),
            internal_operation_results: vec![],
        },
    }
}

#[derive(PartialEq, Debug, NomReader, BinWriter)]
pub enum OperationDataAndMetadata {
    OperationWithMetadata(OperationBatchWithMetadata),
}

#[derive(PartialEq, Debug, NomReader, BinWriter)]
pub struct OperationBatchWithMetadata {
    #[encoding(dynamic, list)]
    pub operations: Vec<OperationWithMetadata>,
    pub signature: UnknownSignature,
}

#[derive(PartialEq, Debug)]
pub struct OperationWithMetadata {
    pub content: ManagerOperationContent,
    pub receipt: OperationResultSum,
}

impl NomReader<'_> for OperationWithMetadata {
    fn nom_read(input: &'_ [u8]) -> tezos_nom::NomResult<'_, Self> {
        let (input, content) = ManagerOperationContent::nom_read(input)?;
        let (input, receipt) = match content {
            ManagerOperationContent::Transfer(_) => {
                let (input, receipt) = OperationResult::<Transfer>::nom_read(input)?;
                (input, OperationResultSum::Transfer(receipt))
            }
            ManagerOperationContent::Reveal(_) => {
                let (input, receipt) = OperationResult::<Reveal>::nom_read(input)?;
                (input, OperationResultSum::Reveal(receipt))
            }
        };
        Ok((input, Self { content, receipt }))
    }
}

impl BinWriter for OperationWithMetadata {
    fn bin_write(&self, output: &mut Vec<u8>) -> tezos_enc::BinResult {
        self.content.bin_write(output)?;
        match &self.receipt {
            OperationResultSum::Transfer(receipt) => receipt.bin_write(output),
            OperationResultSum::Reveal(receipt) => receipt.bin_write(output),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::operation::{ManagerOperation, TransferContent};
    use pretty_assertions::assert_eq;

    fn dummy_test_result_operation() -> OperationDataAndMetadata {
        OperationDataAndMetadata::OperationWithMetadata (
                 OperationBatchWithMetadata {
                    operations: vec![OperationWithMetadata {
                        content: ManagerOperationContent::Transfer(ManagerOperation { source: PublicKeyHash::from_b58check("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx").unwrap(), fee: 468.into(), counter: 1.into(), gas_limit: 2169.into(), storage_limit: 0.into(), operation: TransferContent {
                            amount: 42000000.into(),
                            destination: Contract::from_b58check("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN").unwrap(),
                            parameters: None,
                        } }),
                        receipt: OperationResultSum::Transfer(OperationResult { balance_updates: vec![], result: ContentResult::Applied(
                            TransferTarget::ToContrat(
                            TransferSuccess { storage: None, lazy_storage_diff: None, balance_updates: vec![
                            BalanceUpdate { balance: Balance::Account(Contract::Implicit(PublicKeyHash::from_b58check("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx").unwrap())), changes: -42000000,update_origin : UpdateOrigin::BlockApplication },
                            BalanceUpdate { balance: Balance::Account(Contract::Implicit(PublicKeyHash::from_b58check("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN").unwrap())), changes: 42000000,update_origin : UpdateOrigin::BlockApplication}
                            ], ticket_receipt: vec![], originated_contracts: vec![], consumed_gas: 2169000.into(), storage_size: 0.into(), paid_storage_size_diff: 0.into(), allocated_destination_contract: false }
                            ))

                        , internal_operation_results: vec![] })
                    }],
                    signature:  UnknownSignature::from_base58_check(
                        "sigPc9gwEse2o5nsicnNeWLjLgoMbEGumXw7PErAkMMa1asXVKRq43RPd7TnUKYwuHmejxEu15XTyV1iKGiaa8akFHK7CCEF"
                    ).unwrap(),
                })
    }

    #[test]
    fn test_operation_with_metadata_rlp_roundtrip() {
        let operation_and_receipt = dummy_test_result_operation();
        let mut output = vec![];
        operation_and_receipt
            .bin_write(&mut output)
            .expect("Operation with metadata should be encodable");
        let outputcpy = output.clone();
        let (remaining, read_result) = OperationDataAndMetadata::nom_read(&outputcpy)
            .expect("Operation with metadata should be decodable");

        assert_eq!(
            read_result, operation_and_receipt,
            "Roundtrip failed on {:?}",
            read_result
        );
        assert!(
            remaining.is_empty(),
            "There should be no remaining bytes after decoding"
        );
    }

    // The operation with metadata below is produced by using the following command:
    /* octez-codec encode 022-PsRiotum.operation.data_and_metadata from
    '{"contents":
        [{"kind":"transaction","source":"tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx",
          "fee":"468","counter":"1","gas_limit":"2169","storage_limit":"0",
          "amount":"42000000","destination":"tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN",
          "metadata":{"balance_updates":[],
          "operation_result":{"status":"applied",
          "balance_updates":[{"kind":"contract","contract":"tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx","change":"-42000000","origin":"block"},{"kind":"contract","contract":"tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN","change":"42000000","origin":"block"}],
          "consumed_milligas":"2169000"}}}],
          "signature":"sigPc9gwEse2o5nsicnNeWLjLgoMbEGumXw7PErAkMMa1asXVKRq43RPd7TnUKYwuHmejxEu15XTyV1iKGiaa8akFHK7CCEF"}' */
    #[test]
    fn tezos_compatibility_for_successful_operation_with_metadata() {
        let mut output = vec![];
        dummy_test_result_operation()
            .bin_write(output.as_mut())
            .expect("Operation with metadata should be encodable");
        let operation_and_receipt_bytes = "00000000966c0002298c03ed7d454a101eb7022bc95f7e5f41ac78d40301f9100080bd83140000e7670f32038107a59a2b9cfefae36ea21f5aa63c00000000000000000000004000000002298c03ed7d454a101eb7022bc95f7e5f41ac78fffffffffd7f218000000000e7670f32038107a59a2b9cfefae36ea21f5aa63c000000000280de80000000000000000000a8b1840100000000000000000c5e6f3021d6effcc1b99d918a3db6dd4820893f076386fb9c85bf62f497870936898e970901e5f8b3e41a8eb0aa1a578811c110415c01719e6ed2dc6e96bb0a";

        assert_eq!(hex::encode(output), operation_and_receipt_bytes);
    }

    /*
        octez-codec encode alpha.operation.data_and_metadata from '{
        "contents": [
            {
                "kind": "transaction",
                "source": "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx",
                "fee": "255",
                "counter": "1",
                "gas_limit": "0",
                "storage_limit": "0",
                "amount": "27942405962072064",
                "destination": "tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN",
                "metadata": {
                    "operation_result": {
                        "status": "failed",
                        "errors": []
                    }
                }
            }
        ],
        "signature": "sigvVF2FguUHvZHytQ4AoRn4R6tSMteAt4nEHfYEbwQi3nXa3xvsgE93V1XYL99FYFUAH83iSpcAe7KxGaAeE1tLJ3M2jGJT"
    }'
         */
    #[test]
    fn tezos_compatibility_for_failed_operation_with_metadata() {
        let operation =
            OperationDataAndMetadata::OperationWithMetadata (
                OperationBatchWithMetadata {
                    operations: vec![OperationWithMetadata {
                        content: ManagerOperationContent::Transfer(
                            ManagerOperation {
                                source: PublicKeyHash::from_b58check("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx").unwrap(),
                                fee: 255.into(),
                                counter: 1.into(),
                                gas_limit: 0.into(),
                                storage_limit: 0.into(),
                                operation: TransferContent {
                                    amount: 27942405962072064.into(),
                                    destination: Contract::from_b58check("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN").unwrap(),
                                    parameters: None,
                                }
                            }),
                        receipt: OperationResultSum::Transfer(
                                    OperationResult {
                                        balance_updates: vec![],
                                        result: ContentResult::Failed(vec![].into()),
                                        internal_operation_results: vec![]
                                    }
                                )
                    }],
                    signature:  UnknownSignature::from_base58_check(
                       "sigvVF2FguUHvZHytQ4AoRn4R6tSMteAt4nEHfYEbwQi3nXa3xvsgE93V1XYL99FYFUAH83iSpcAe7KxGaAeE1tLJ3M2jGJT"
                    ).unwrap(),
                }
            );
        let mut output = vec![];
        operation
            .bin_write(output.as_mut())
            .expect("Operation with metadata should be encodable");
        let operation_and_receipt_bytes = "00000000476c0002298c03ed7d454a101eb7022bc95f7e5f41ac78ff010100008080a8ec85afd1310000e7670f32038107a59a2b9cfefae36ea21f5aa63c0000000000010000000000000000f868f45f51a0c7a5733ab2c7f29781303904d9ef5b8fbf7b96429f00fe487c1d0f174c205b49c7393e5436b9522b88d3951113c32115e518465e029439874306";

        assert_eq!(hex::encode(output), operation_and_receipt_bytes);
    }
}
