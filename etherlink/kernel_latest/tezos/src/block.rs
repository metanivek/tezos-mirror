// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use crate::enc_wrappers::{BlockHash, BlockNumber, OperationHash};
use crate::operation_result::OperationDataAndMetadata;
use nom::error::ParseError;
use nom::Finish;
use primitive_types::H256;
use tezos_crypto_rs::blake2b::digest_256;
use tezos_data_encoding::enc as tezos_enc;
use tezos_data_encoding::nom::error::DecodeError;
use tezos_data_encoding::nom::{self as tezos_nom};
use tezos_enc::{BinError, BinWriter};
use tezos_nom::NomReader;
use tezos_smart_rollup::types::Timestamp;

#[derive(PartialEq, Debug, BinWriter, NomReader)]
pub struct AppliedOperation {
    // OperationHash are 32 bytes long
    pub hash: OperationHash,
    pub branch: BlockHash,
    #[encoding(dynamic)]
    pub op_and_receipt: OperationDataAndMetadata,
}

// WIP: This structure will evolve to look like Tezos block
#[derive(PartialEq, Debug, BinWriter, NomReader)]
pub struct TezBlock {
    pub hash: BlockHash,
    pub number: BlockNumber,
    pub previous_hash: BlockHash,
    pub timestamp: Timestamp,
    #[encoding(dynamic, list)]
    pub operations: Vec<AppliedOperation>,
}

impl TezBlock {
    pub fn genesis_block_hash() -> H256 {
        // This H256 comes from this b58 hash 'BLockGenesisGenesisGenesisGenesisGenesis1db77eJNeJ9'
        // That is the ghostnet genesis hash according to 'devtools/get_contracts/config.ml'
        H256::from_slice(
            &hex::decode(
                "8fcf233671b6a04fcf679d2a381c2544ea6c1ea29ba6157776ed8423e7c02934",
            )
            .unwrap(),
        )
    }

    // This function must be used on a TezBlock whose hash field is H256::zero()
    fn hash(&self) -> Result<BlockHash, BinError> {
        let mut encoded_data = vec![];
        self.bin_write(&mut encoded_data)?;
        let hashed_data = digest_256(&encoded_data);
        Ok(BlockHash(H256::from_slice(&hashed_data)))
    }

    pub fn new(
        number: BlockNumber,
        timestamp: Timestamp,
        previous_hash: H256,
        operations: Vec<AppliedOperation>,
    ) -> Result<Self, BinError> {
        let block = Self {
            hash: BlockHash(H256::zero()), // Placeholder, will be computed
            number,
            timestamp,
            previous_hash: BlockHash(previous_hash),
            operations,
        };
        Ok(Self {
            hash: block.hash()?,
            ..block
        })
    }

    pub fn to_bytes(&self) -> Result<Vec<u8>, BinError> {
        let mut output = vec![];
        self.bin_write(&mut output)?;
        Ok(output)
    }

    pub fn try_from_bytes(bytes: &[u8]) -> Result<Self, DecodeError<&[u8]>> {
        let (remaining, block) = Self::nom_read(bytes).finish()?;
        if !remaining.is_empty() {
            return Err(DecodeError::from_error_kind(
                remaining,
                nom::error::ErrorKind::NonEmpty,
            ));
        }
        Ok(block)
    }
}

#[cfg(test)]
mod tests {
    use primitive_types::H256;
    use tezos_smart_rollup::types::Timestamp;

    use crate::operation_result::{
        OperationBatchWithMetadata, OperationDataAndMetadata, OperationResult,
        OperationResultSum, OperationWithMetadata, RevealSuccess,
    };

    use super::{AppliedOperation, TezBlock};

    pub fn block_roundtrip(block: TezBlock) {
        let bytes = block
            .to_bytes()
            .expect("Block encoding should have succeeded");
        let decoded_block =
            TezBlock::try_from_bytes(&bytes).expect("Block should be decodable");
        assert_eq!(block, decoded_block, "Roundtrip failed on {:?}", block)
    }

    fn dummy_applied_operation() -> AppliedOperation {
        let hash = H256::random().into();
        let data = crate::operation::make_dummy_reveal_operation();
        let receipt = OperationResultSum::Reveal(OperationResult {
            balance_updates: vec![],
            result: crate::operation_result::ContentResult::Applied(RevealSuccess {
                consumed_gas: 0u64.into(),
            }),
            internal_operation_results: vec![],
        });
        AppliedOperation {
            hash,
            branch: data.branch,
            op_and_receipt: OperationDataAndMetadata::OperationWithMetadata(
                OperationBatchWithMetadata {
                    operations: vec![OperationWithMetadata {
                        content: data.content,
                        receipt,
                    }],
                    signature: data.signature,
                },
            ),
        }
    }

    fn dummy_tezblock(operations: Vec<AppliedOperation>) -> TezBlock {
        let number = 1u32.into();
        let timestamp = Timestamp::from(0);
        let previous_hash = TezBlock::genesis_block_hash();
        TezBlock::new(number, timestamp, previous_hash, operations)
            .expect("Block creation should have succeeded")
    }

    #[test]
    fn test_empty_block_rlp_roundtrip() {
        block_roundtrip(dummy_tezblock(vec![]));
    }

    #[test]
    fn test_block_rlp_roundtrip() {
        block_roundtrip(dummy_tezblock(vec![
            dummy_applied_operation(),
            dummy_applied_operation(),
        ]));
    }
}
