// Copyright (c) SimpleStaking, Viable Systems and Tezedge Contributors
// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use cryptoxide::blake2b::Blake2b;
use cryptoxide::digest::Digest;
use serde::{Deserialize, Serialize};
use thiserror::Error;

#[derive(Serialize, Deserialize, Error, Debug, PartialEq, Eq, Clone, Copy)]
pub enum Blake2bError {
    #[error("Output digest length must be between 16 and 64 bytes.")]
    InvalidLength,
}

/// Generate digest of length 256 bits (32bytes) from arbitrary binary data
pub fn digest_256(data: &[u8]) -> Vec<u8> {
    digest(data, 32).unwrap()
}

// Generate digest of length 160 bits (20bytes) from arbitrary binary data
pub fn digest_160(data: &[u8]) -> Vec<u8> {
    digest(data, 20).unwrap()
}

/// Generate digest of length 128 bits (16bytes) from arbitrary binary data
pub fn digest_128(data: &[u8]) -> Vec<u8> {
    digest(data, 16).unwrap()
}

/// Arbitrary Blake2b digest generation from generic data.
// Should be noted, that base Blake2b supports arbitrary digest length from 16 to 64 bytes
pub fn digest(data: &[u8], out_len: usize) -> Result<Vec<u8>, Blake2bError> {
    if !(16..=64).contains(&out_len) {
        return Err(Blake2bError::InvalidLength);
    }

    let mut hasher = Blake2b::new(out_len);

    hasher.input(data);

    let mut result = vec![0; hasher.output_bytes()];

    hasher.result(result.as_mut_slice());

    Ok(result)
}

/// Arbitrary Blake2b digest generation from pieces of generic data.
// Should be noted, that base Blake2b supports arbitrary digest length from 16 to 64 bytes
pub fn digest_all<T, I>(data: T, out_len: usize) -> Result<Vec<u8>, Blake2bError>
where
    T: IntoIterator<Item = I>,
    I: AsRef<[u8]>,
{
    if !(16..=64).contains(&out_len) {
        return Err(Blake2bError::InvalidLength);
    }

    let mut hasher = Blake2b::new(out_len);
    for d in data.into_iter() {
        hasher.input(d.as_ref());
    }

    let mut result = vec![0; hasher.output_bytes()];

    hasher.result(result.as_mut_slice());

    Ok(result)
}

/// Computes a full binary tree from the list [xs].
/// In this tree the ith leaf (from left to right) is the ith element of the
/// list [xs]. If [xs] is the empty list, then the result is the empty tree. If
/// the length of [xs] is not a power of 2, then the tree is padded with leaves
/// containing the last element of [xs] such that a full tree is obtained.
///
// Example: given the list [1, 2, 3, 4, 5], the tree
//
//         /\
//        /  \
//       /    \
//      /      \
//     /\      /\
//    /  \    /  \
//   /\  /\  /\  /\
//  1 2  3 4 5 5 5 5
//
//
// TODO: optimize it,
// this implementation will calculate the same hash [5, 5] two times.
pub fn merkle_tree<Leaf>(list: &[Leaf]) -> Vec<u8>
where
    Leaf: AsRef<[u8]>,
{
    use std::ops::{Index, RangeFrom, RangeTo};

    // Helper for calculating merkle tree
    // The wrapper around slice which repeats last item forever
    struct RepeatingSlice<'a, Leaf>(pub &'a [Leaf]);

    impl<Leaf> Index<usize> for RepeatingSlice<'_, Leaf> {
        type Output = Leaf;

        fn index(&self, index: usize) -> &Self::Output {
            if self.0.is_empty() {
                panic!();
            } else if index < self.0.len() {
                self.0.index(index)
            } else {
                self.0.last().unwrap()
            }
        }
    }

    impl<Leaf> Index<RangeFrom<usize>> for RepeatingSlice<'_, Leaf> {
        type Output = [Leaf];

        fn index(&self, index: RangeFrom<usize>) -> &Self::Output {
            if self.0.is_empty() {
                panic!();
            } else if index.start < self.0.len() {
                &self.0[index]
            } else {
                &self.0[(self.0.len() - 1)..]
            }
        }
    }

    impl<Leaf> Index<RangeTo<usize>> for RepeatingSlice<'_, Leaf> {
        type Output = [Leaf];

        fn index(&self, index: RangeTo<usize>) -> &Self::Output {
            if self.0.is_empty() {
                panic!();
            } else if index.end <= self.0.len() {
                &self.0[index]
            } else {
                &self.0[..self.0.len()]
            }
        }
    }

    fn merkle_tree_inner<Leaf>(list: &RepeatingSlice<Leaf>, degree: u32) -> Vec<u8>
    where
        Leaf: AsRef<[u8]>,
    {
        match degree {
            0 => digest_256(list[0].as_ref()),
            d => {
                let middle = 1 << (d - 1);
                digest_all(
                    [
                        merkle_tree_inner(&RepeatingSlice(&list[..middle]), d - 1),
                        merkle_tree_inner(&RepeatingSlice(&list[middle..]), d - 1),
                    ],
                    32,
                )
                .unwrap() // we know length is within bounds, and that's the only error possible
            }
        }
    }

    if list.is_empty() {
        digest_256(&[])
    } else {
        merkle_tree_inner(&RepeatingSlice(list), 64 - (list.len() - 1).leading_zeros())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn blake2b_256() {
        let hash = digest_256(b"hello world");
        let expected =
            hex::decode("256c83b297114d201b30179f3f0ef0cace9783622da5974326b436178aeef610")
                .unwrap();
        assert_eq!(expected, hash)
    }

    #[test]
    fn blake2b_128() {
        let hash = digest_128(b"hello world");
        let expected = hex::decode("e9a804b2e527fd3601d2ffc0bb023cd6").unwrap();
        assert_eq!(expected, hash);
    }

    #[test]
    fn blake2b_less_than_128() {
        // This should fail, as blake2b does not support hashes shorter than 16 bytes.
        assert!(digest(b"hello world", 15).is_err())
    }

    #[test]
    fn blake2b_more_than_512() {
        // This should fail, as blake2b does not support hashes longer than 64 bytes.
        assert!(digest(b"hello world", 65).is_err())
    }

    #[test]
    fn blake2b_digest() {
        let hash = digest(b"hello world", 32).unwrap();
        assert_eq!(
            hash,
            hex::decode("256c83b297114d201b30179f3f0ef0cace9783622da5974326b436178aeef610")
                .unwrap()
        );
    }

    #[test]
    fn blake2b_digest_all() {
        let hash = digest_all(["hello", " ", "world"], 32).unwrap();
        assert_eq!(
            hash,
            hex::decode("256c83b297114d201b30179f3f0ef0cace9783622da5974326b436178aeef610")
                .unwrap()
        );
    }
}
