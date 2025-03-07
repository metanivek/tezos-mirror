meta:
  id: id_014__ptkathma__operation__contents
  endian: be
  imports:
  - block_header__shell
  - operation__shell_header
doc: ! 'Encoding id: 014-PtKathma.operation.contents'
types:
  activate_account:
    seq:
    - id: pkh
      size: 20
    - id: secret
      size: 20
  after:
    seq:
    - id: after_tag
      type: u1
      enum: after_tag
    - id: value
      size: 32
      if: (after_tag == after_tag::value)
    - id: node
      size: 32
      if: (after_tag == after_tag::node)
  amount:
    seq:
    - id: amount_tag
      type: u1
      enum: amount_tag
    - id: small
      type: u1
      if: (amount_tag == amount_tag::small)
    - id: medium
      type: u2be
      if: (amount_tag == amount_tag::medium)
    - id: biggish
      type: s4be
      if: (amount_tag == amount_tag::biggish)
    - id: bigger
      type: s8be
      if: (amount_tag == amount_tag::bigger)
  arithmetic__pvm__with__proof:
    seq:
    - id: tree_proof
      type: tree_proof
    - id: given
      type: given
    - id: requested
      type: requested
  back_pointers:
    seq:
    - id: back_pointers_entries
      type: back_pointers_entries
      repeat: eos
  back_pointers_0:
    seq:
    - id: len_back_pointers
      type: u4be
      valid:
        max: 1073741823
    - id: back_pointers
      type: back_pointers
      size: len_back_pointers
  back_pointers_entries:
    seq:
    - id: inbox_hash
      size: 32
  ballot:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: period
      type: s4be
    - id: proposal
      size: 32
    - id: ballot
      type: s1
  before:
    seq:
    - id: before_tag
      type: u1
      enum: before_tag
    - id: value
      size: 32
      if: (before_tag == before_tag::value)
    - id: node
      size: 32
      if: (before_tag == before_tag::node)
  bh1:
    seq:
    - id: id_014__ptkathma__block_header__alpha__full_header
      type: id_014__ptkathma__block_header__alpha__full_header
  bh1_0:
    seq:
    - id: len_bh1
      type: u4be
      valid:
        max: 1073741823
    - id: bh1
      type: bh1
      size: len_bh1
  bh2:
    seq:
    - id: id_014__ptkathma__block_header__alpha__full_header
      type: id_014__ptkathma__block_header__alpha__full_header
  bh2_0:
    seq:
    - id: len_bh2
      type: u4be
      valid:
        max: 1073741823
    - id: bh2
      type: bh2
      size: len_bh2
  bytes_dyn_uint30:
    seq:
    - id: len_bytes_dyn_uint30
      type: u4be
      valid:
        max: 1073741823
    - id: bytes_dyn_uint30
      size: len_bytes_dyn_uint30
  case_0:
    seq:
    - id: case_0_field0
      type: s2be
    - id: case_0_field1
      size: 32
      doc: context_hash
    - id: case_0_field2
      size: 32
      doc: context_hash
    - id: case_0_field3
      type: case_0_field3_0
  case_0_field3:
    seq:
    - id: case_0_field3_entries
      type: case_0_field3_entries
      repeat: eos
  case_0_field3_0:
    seq:
    - id: len_case_0_field3
      type: u4be
      valid:
        max: 1073741823
    - id: case_0_field3
      type: case_0_field3
      size: len_case_0_field3
  case_0_field3_entries:
    seq:
    - id: case_0_field3_elt_tag
      type: u1
      enum: case_0_field3_elt_tag
    - id: inode
      type: u1
      if: (case_0_field3_elt_tag == case_0_field3_elt_tag::inode)
    - id: inode
      type: inode
      if: (case_0_field3_elt_tag == case_0_field3_elt_tag::inode)
    - id: inode
      type: inode_0
      if: (case_0_field3_elt_tag == case_0_field3_elt_tag::inode)
    - id: inode
      type: inode_1
      if: (case_0_field3_elt_tag == case_0_field3_elt_tag::inode)
    - id: inode
      type: u2be
      if: (case_0_field3_elt_tag == case_0_field3_elt_tag::inode)
    - id: inode
      type: inode_2
      if: (case_0_field3_elt_tag == case_0_field3_elt_tag::inode)
    - id: inode
      type: inode_3
      if: (case_0_field3_elt_tag == case_0_field3_elt_tag::inode)
    - id: inode
      type: inode_4
      if: (case_0_field3_elt_tag == case_0_field3_elt_tag::inode)
    - id: inode
      type: s4be
      if: (case_0_field3_elt_tag == case_0_field3_elt_tag::inode)
    - id: inode
      type: inode_5
      if: (case_0_field3_elt_tag == case_0_field3_elt_tag::inode)
    - id: inode
      type: inode_6
      if: (case_0_field3_elt_tag == case_0_field3_elt_tag::inode)
    - id: inode
      type: inode_7
      if: (case_0_field3_elt_tag == case_0_field3_elt_tag::inode)
    - id: inode
      type: s8be
      if: (case_0_field3_elt_tag == case_0_field3_elt_tag::inode)
    - id: inode
      type: inode_8
      if: (case_0_field3_elt_tag == case_0_field3_elt_tag::inode)
    - id: inode
      type: inode_9
      if: (case_0_field3_elt_tag == case_0_field3_elt_tag::inode)
    - id: inode
      type: inode_10
      if: (case_0_field3_elt_tag == case_0_field3_elt_tag::inode)
    - id: other_elts
      type: other_elts_entries
      if: (case_0_field3_elt_tag == case_0_field3_elt_tag::other_elts)
    - id: other_elts
      type: other_elts_entries
      if: (case_0_field3_elt_tag == case_0_field3_elt_tag::other_elts)
    - id: other_elts
      type: other_elts_0
      if: (case_0_field3_elt_tag == case_0_field3_elt_tag::other_elts)
    - id: other_elts
      type: other_elts_2
      if: (case_0_field3_elt_tag == case_0_field3_elt_tag::other_elts)
    - id: other_elts
      type: other_elts_3
      if: (case_0_field3_elt_tag == case_0_field3_elt_tag::other_elts)
    - id: other_elts
      type: bytes_dyn_uint30
      if: (case_0_field3_elt_tag == case_0_field3_elt_tag::other_elts)
    - id: other_elts
      type: other_elts_4
      if: (case_0_field3_elt_tag == case_0_field3_elt_tag::other_elts)
    - id: other_elts
      type: other_elts_5
      if: (case_0_field3_elt_tag == case_0_field3_elt_tag::other_elts)
    - id: other_elts
      type: other_elts_6
      if: (case_0_field3_elt_tag == case_0_field3_elt_tag::other_elts)
    - id: other_elts
      type: other_elts_7
      if: (case_0_field3_elt_tag == case_0_field3_elt_tag::other_elts)
  case_1:
    seq:
    - id: case_1_field0
      type: s2be
    - id: case_1_field1
      size: 32
      doc: context_hash
    - id: case_1_field2
      size: 32
      doc: context_hash
    - id: case_1_field3
      type: case_1_field3_0
  case_1_field3:
    seq:
    - id: case_1_field3_entries
      type: case_1_field3_entries
      repeat: eos
  case_1_field3_0:
    seq:
    - id: len_case_1_field3
      type: u4be
      valid:
        max: 1073741823
    - id: case_1_field3
      type: case_1_field3
      size: len_case_1_field3
  case_1_field3_entries:
    seq:
    - id: case_1_field3_elt_tag
      type: u1
      enum: case_1_field3_elt_tag
    - id: inode
      type: u1
      if: (case_1_field3_elt_tag == case_1_field3_elt_tag::inode)
    - id: inode
      type: inode
      if: (case_1_field3_elt_tag == case_1_field3_elt_tag::inode)
    - id: inode
      type: inode_0
      if: (case_1_field3_elt_tag == case_1_field3_elt_tag::inode)
    - id: inode
      type: inode_1
      if: (case_1_field3_elt_tag == case_1_field3_elt_tag::inode)
    - id: inode
      type: u2be
      if: (case_1_field3_elt_tag == case_1_field3_elt_tag::inode)
    - id: inode
      type: inode_2
      if: (case_1_field3_elt_tag == case_1_field3_elt_tag::inode)
    - id: inode
      type: inode_3
      if: (case_1_field3_elt_tag == case_1_field3_elt_tag::inode)
    - id: inode
      type: inode_4
      if: (case_1_field3_elt_tag == case_1_field3_elt_tag::inode)
    - id: inode
      type: s4be
      if: (case_1_field3_elt_tag == case_1_field3_elt_tag::inode)
    - id: inode
      type: inode_5
      if: (case_1_field3_elt_tag == case_1_field3_elt_tag::inode)
    - id: inode
      type: inode_6
      if: (case_1_field3_elt_tag == case_1_field3_elt_tag::inode)
    - id: inode
      type: inode_7
      if: (case_1_field3_elt_tag == case_1_field3_elt_tag::inode)
    - id: inode
      type: s8be
      if: (case_1_field3_elt_tag == case_1_field3_elt_tag::inode)
    - id: inode
      type: inode_8
      if: (case_1_field3_elt_tag == case_1_field3_elt_tag::inode)
    - id: inode
      type: inode_9
      if: (case_1_field3_elt_tag == case_1_field3_elt_tag::inode)
    - id: inode
      type: inode_10
      if: (case_1_field3_elt_tag == case_1_field3_elt_tag::inode)
    - id: other_elts
      type: other_elts_entries
      if: (case_1_field3_elt_tag == case_1_field3_elt_tag::other_elts)
    - id: other_elts
      type: other_elts_entries
      if: (case_1_field3_elt_tag == case_1_field3_elt_tag::other_elts)
    - id: other_elts
      type: other_elts_0
      if: (case_1_field3_elt_tag == case_1_field3_elt_tag::other_elts)
    - id: other_elts
      type: other_elts_2
      if: (case_1_field3_elt_tag == case_1_field3_elt_tag::other_elts)
    - id: other_elts
      type: other_elts_3
      if: (case_1_field3_elt_tag == case_1_field3_elt_tag::other_elts)
    - id: other_elts
      type: bytes_dyn_uint30
      if: (case_1_field3_elt_tag == case_1_field3_elt_tag::other_elts)
    - id: other_elts
      type: other_elts_4
      if: (case_1_field3_elt_tag == case_1_field3_elt_tag::other_elts)
    - id: other_elts
      type: other_elts_5
      if: (case_1_field3_elt_tag == case_1_field3_elt_tag::other_elts)
    - id: other_elts
      type: other_elts_6
      if: (case_1_field3_elt_tag == case_1_field3_elt_tag::other_elts)
    - id: other_elts
      type: other_elts_7
      if: (case_1_field3_elt_tag == case_1_field3_elt_tag::other_elts)
  case_2:
    seq:
    - id: case_2_field0
      type: s2be
    - id: case_2_field1
      size: 32
      doc: context_hash
    - id: case_2_field2
      size: 32
      doc: context_hash
    - id: case_2_field3
      type: case_2_field3_0
  case_2_field3:
    seq:
    - id: case_2_field3_entries
      type: case_2_field3_entries
      repeat: eos
  case_2_field3_0:
    seq:
    - id: len_case_2_field3
      type: u4be
      valid:
        max: 1073741823
    - id: case_2_field3
      type: case_2_field3
      size: len_case_2_field3
  case_2_field3_entries:
    seq:
    - id: case_2_field3_elt_tag
      type: u1
      enum: case_2_field3_elt_tag
    - id: inode
      type: u1
      if: (case_2_field3_elt_tag == case_2_field3_elt_tag::inode)
    - id: inode
      type: inode
      if: (case_2_field3_elt_tag == case_2_field3_elt_tag::inode)
    - id: inode
      type: inode_0
      if: (case_2_field3_elt_tag == case_2_field3_elt_tag::inode)
    - id: inode
      type: inode_1
      if: (case_2_field3_elt_tag == case_2_field3_elt_tag::inode)
    - id: inode
      type: u2be
      if: (case_2_field3_elt_tag == case_2_field3_elt_tag::inode)
    - id: inode
      type: inode_2
      if: (case_2_field3_elt_tag == case_2_field3_elt_tag::inode)
    - id: inode
      type: inode_3
      if: (case_2_field3_elt_tag == case_2_field3_elt_tag::inode)
    - id: inode
      type: inode_4
      if: (case_2_field3_elt_tag == case_2_field3_elt_tag::inode)
    - id: inode
      type: s4be
      if: (case_2_field3_elt_tag == case_2_field3_elt_tag::inode)
    - id: inode
      type: inode_5
      if: (case_2_field3_elt_tag == case_2_field3_elt_tag::inode)
    - id: inode
      type: inode_6
      if: (case_2_field3_elt_tag == case_2_field3_elt_tag::inode)
    - id: inode
      type: inode_7
      if: (case_2_field3_elt_tag == case_2_field3_elt_tag::inode)
    - id: inode
      type: s8be
      if: (case_2_field3_elt_tag == case_2_field3_elt_tag::inode)
    - id: inode
      type: inode_8
      if: (case_2_field3_elt_tag == case_2_field3_elt_tag::inode)
    - id: inode
      type: inode_9
      if: (case_2_field3_elt_tag == case_2_field3_elt_tag::inode)
    - id: inode
      type: inode_10
      if: (case_2_field3_elt_tag == case_2_field3_elt_tag::inode)
    - id: other_elts
      type: other_elts_entries
      if: (case_2_field3_elt_tag == case_2_field3_elt_tag::other_elts)
    - id: other_elts
      type: other_elts_entries
      if: (case_2_field3_elt_tag == case_2_field3_elt_tag::other_elts)
    - id: other_elts
      type: other_elts_0
      if: (case_2_field3_elt_tag == case_2_field3_elt_tag::other_elts)
    - id: other_elts
      type: other_elts_2
      if: (case_2_field3_elt_tag == case_2_field3_elt_tag::other_elts)
    - id: other_elts
      type: other_elts_3
      if: (case_2_field3_elt_tag == case_2_field3_elt_tag::other_elts)
    - id: other_elts
      type: bytes_dyn_uint30
      if: (case_2_field3_elt_tag == case_2_field3_elt_tag::other_elts)
    - id: other_elts
      type: other_elts_4
      if: (case_2_field3_elt_tag == case_2_field3_elt_tag::other_elts)
    - id: other_elts
      type: other_elts_5
      if: (case_2_field3_elt_tag == case_2_field3_elt_tag::other_elts)
    - id: other_elts
      type: other_elts_6
      if: (case_2_field3_elt_tag == case_2_field3_elt_tag::other_elts)
    - id: other_elts
      type: other_elts_7
      if: (case_2_field3_elt_tag == case_2_field3_elt_tag::other_elts)
  case_3:
    seq:
    - id: case_3_field0
      type: s2be
    - id: case_3_field1
      size: 32
      doc: context_hash
    - id: case_3_field2
      size: 32
      doc: context_hash
    - id: case_3_field3
      type: case_3_field3_0
  case_3_field3:
    seq:
    - id: case_3_field3_entries
      type: case_3_field3_entries
      repeat: eos
  case_3_field3_0:
    seq:
    - id: len_case_3_field3
      type: u4be
      valid:
        max: 1073741823
    - id: case_3_field3
      type: case_3_field3
      size: len_case_3_field3
  case_3_field3_entries:
    seq:
    - id: case_3_field3_elt_tag
      type: u1
      enum: case_3_field3_elt_tag
    - id: inode
      type: u1
      if: (case_3_field3_elt_tag == case_3_field3_elt_tag::inode)
    - id: inode
      type: inode
      if: (case_3_field3_elt_tag == case_3_field3_elt_tag::inode)
    - id: inode
      type: inode_0
      if: (case_3_field3_elt_tag == case_3_field3_elt_tag::inode)
    - id: inode
      type: inode_1
      if: (case_3_field3_elt_tag == case_3_field3_elt_tag::inode)
    - id: inode
      type: u2be
      if: (case_3_field3_elt_tag == case_3_field3_elt_tag::inode)
    - id: inode
      type: inode_2
      if: (case_3_field3_elt_tag == case_3_field3_elt_tag::inode)
    - id: inode
      type: inode_3
      if: (case_3_field3_elt_tag == case_3_field3_elt_tag::inode)
    - id: inode
      type: inode_4
      if: (case_3_field3_elt_tag == case_3_field3_elt_tag::inode)
    - id: inode
      type: s4be
      if: (case_3_field3_elt_tag == case_3_field3_elt_tag::inode)
    - id: inode
      type: inode_5
      if: (case_3_field3_elt_tag == case_3_field3_elt_tag::inode)
    - id: inode
      type: inode_6
      if: (case_3_field3_elt_tag == case_3_field3_elt_tag::inode)
    - id: inode
      type: inode_7
      if: (case_3_field3_elt_tag == case_3_field3_elt_tag::inode)
    - id: inode
      type: s8be
      if: (case_3_field3_elt_tag == case_3_field3_elt_tag::inode)
    - id: inode
      type: inode_8
      if: (case_3_field3_elt_tag == case_3_field3_elt_tag::inode)
    - id: inode
      type: inode_9
      if: (case_3_field3_elt_tag == case_3_field3_elt_tag::inode)
    - id: inode
      type: inode_10
      if: (case_3_field3_elt_tag == case_3_field3_elt_tag::inode)
    - id: other_elts
      type: other_elts_entries
      if: (case_3_field3_elt_tag == case_3_field3_elt_tag::other_elts)
    - id: other_elts
      type: other_elts_entries
      if: (case_3_field3_elt_tag == case_3_field3_elt_tag::other_elts)
    - id: other_elts
      type: other_elts_0
      if: (case_3_field3_elt_tag == case_3_field3_elt_tag::other_elts)
    - id: other_elts
      type: other_elts_2
      if: (case_3_field3_elt_tag == case_3_field3_elt_tag::other_elts)
    - id: other_elts
      type: other_elts_3
      if: (case_3_field3_elt_tag == case_3_field3_elt_tag::other_elts)
    - id: other_elts
      type: bytes_dyn_uint30
      if: (case_3_field3_elt_tag == case_3_field3_elt_tag::other_elts)
    - id: other_elts
      type: other_elts_4
      if: (case_3_field3_elt_tag == case_3_field3_elt_tag::other_elts)
    - id: other_elts
      type: other_elts_5
      if: (case_3_field3_elt_tag == case_3_field3_elt_tag::other_elts)
    - id: other_elts
      type: other_elts_6
      if: (case_3_field3_elt_tag == case_3_field3_elt_tag::other_elts)
    - id: other_elts
      type: other_elts_7
      if: (case_3_field3_elt_tag == case_3_field3_elt_tag::other_elts)
  commitment:
    seq:
    - id: level
      type: s4be
    - id: messages
      type: messages_0
    - id: predecessor
      type: predecessor
    - id: inbox_merkle_root
      size: 32
  commitment_0:
    seq:
    - id: compressed_state
      size: 32
    - id: inbox_level
      type: s4be
    - id: predecessor
      size: 32
    - id: number_of_messages
      type: s4be
    - id: number_of_ticks
      type: s4be
  dal_publish_slot_header:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_014__ptkathma__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: slot
      type: slot
  dal_slot_availability:
    seq:
    - id: endorser
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: endorsement
      type: z
  delegation:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_014__ptkathma__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: delegate_tag
      type: u1
      enum: bool
    - id: delegate
      type: public_key_hash
      if: (delegate_tag == bool::true)
      doc: A Ed25519, Secp256k1, or P256 public key hash
  dense_proof_entries:
    seq:
    - id: dense_proof_elt
      type: inode_tree
  deposit:
    seq:
    - id: sender
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: destination
      size: 20
    - id: ticket_hash
      size: 32
    - id: amount
      type: amount
  dissection:
    seq:
    - id: dissection_entries
      type: dissection_entries
      repeat: eos
  dissection_0:
    seq:
    - id: len_dissection
      type: u4be
      valid:
        max: 1073741823
    - id: dissection
      type: dissection
      size: len_dissection
  dissection_elt_field0:
    seq:
    - id: dissection_elt_field0_tag
      type: u1
      enum: dissection_elt_field0_tag
    - id: some
      size: 32
      if: (dissection_elt_field0_tag == dissection_elt_field0_tag::some)
  dissection_entries:
    seq:
    - id: dissection_elt_field0
      type: dissection_elt_field0
    - id: dissection_elt_field1
      type: n
  double_baking_evidence:
    seq:
    - id: bh1
      type: bh1_0
    - id: bh2
      type: bh2_0
  double_endorsement_evidence:
    seq:
    - id: op1
      type: op1_0
    - id: op2
      type: op2_0
  double_preendorsement_evidence:
    seq:
    - id: op1
      type: op1_2
    - id: op2
      type: op2_2
  endorsement:
    seq:
    - id: slot
      type: u2be
    - id: level
      type: s4be
    - id: round
      type: s4be
    - id: block_payload_hash
      size: 32
  extender:
    seq:
    - id: length
      type: s8be
    - id: segment
      type: segment_0
    - id: proof
      type: inode_tree
  first_after:
    seq:
    - id: first_after_field0
      type: s4be
    - id: first_after_field1
      type: n
  given:
    seq:
    - id: given_tag
      type: u1
      enum: given_tag
    - id: some
      type: some
      if: (given_tag == given_tag::some)
  id_014__ptkathma__block_header__alpha__full_header:
    seq:
    - id: id_014__ptkathma__block_header__alpha__full_header
      type: block_header__shell
    - id: id_014__ptkathma__block_header__alpha__signed_contents
      type: id_014__ptkathma__block_header__alpha__signed_contents
  id_014__ptkathma__block_header__alpha__signed_contents:
    seq:
    - id: id_014__ptkathma__block_header__alpha__unsigned_contents
      type: id_014__ptkathma__block_header__alpha__unsigned_contents
    - id: signature
      size: 64
  id_014__ptkathma__block_header__alpha__unsigned_contents:
    seq:
    - id: payload_hash
      size: 32
    - id: payload_round
      type: s4be
    - id: proof_of_work_nonce
      size: 8
    - id: seed_nonce_hash_tag
      type: u1
      enum: bool
    - id: seed_nonce_hash
      size: 32
      if: (seed_nonce_hash_tag == bool::true)
    - id: liquidity_baking_toggle_vote
      type: id_014__ptkathma__liquidity_baking_toggle_vote
  id_014__ptkathma__contract_id:
    seq:
    - id: id_014__ptkathma__contract_id_tag
      type: u1
      enum: id_014__ptkathma__contract_id_tag
    - id: implicit
      type: public_key_hash
      if: (id_014__ptkathma__contract_id_tag == id_014__ptkathma__contract_id_tag::implicit)
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: originated
      type: originated
      if: (id_014__ptkathma__contract_id_tag == id_014__ptkathma__contract_id_tag::originated)
  id_014__ptkathma__contract_id__originated:
    seq:
    - id: id_014__ptkathma__contract_id__originated_tag
      type: u1
      enum: id_014__ptkathma__contract_id__originated_tag
    - id: originated
      type: originated
      if: (id_014__ptkathma__contract_id__originated_tag == id_014__ptkathma__contract_id__originated_tag::originated)
  id_014__ptkathma__entrypoint:
    seq:
    - id: id_014__ptkathma__entrypoint_tag
      type: u1
      enum: id_014__ptkathma__entrypoint_tag
    - id: named
      type: named_0
      if: (id_014__ptkathma__entrypoint_tag == id_014__ptkathma__entrypoint_tag::named)
  id_014__ptkathma__inlined__endorsement:
    seq:
    - id: id_014__ptkathma__inlined__endorsement
      type: operation__shell_header
    - id: operations
      type: id_014__ptkathma__inlined__endorsement_mempool__contents
    - id: signature_tag
      type: u1
      enum: bool
    - id: signature
      size: 64
      if: (signature_tag == bool::true)
  id_014__ptkathma__inlined__endorsement_mempool__contents:
    seq:
    - id: id_014__ptkathma__inlined__endorsement_mempool__contents_tag
      type: u1
      enum: id_014__ptkathma__inlined__endorsement_mempool__contents_tag
    - id: endorsement
      type: endorsement
      if: (id_014__ptkathma__inlined__endorsement_mempool__contents_tag == id_014__ptkathma__inlined__endorsement_mempool__contents_tag::endorsement)
  id_014__ptkathma__inlined__preendorsement:
    seq:
    - id: id_014__ptkathma__inlined__preendorsement
      type: operation__shell_header
    - id: operations
      type: id_014__ptkathma__inlined__preendorsement__contents
    - id: signature_tag
      type: u1
      enum: bool
    - id: signature
      size: 64
      if: (signature_tag == bool::true)
  id_014__ptkathma__inlined__preendorsement__contents:
    seq:
    - id: id_014__ptkathma__inlined__preendorsement__contents_tag
      type: u1
      enum: id_014__ptkathma__inlined__preendorsement__contents_tag
    - id: preendorsement
      type: preendorsement
      if: (id_014__ptkathma__inlined__preendorsement__contents_tag == id_014__ptkathma__inlined__preendorsement__contents_tag::preendorsement)
  id_014__ptkathma__liquidity_baking_toggle_vote:
    seq:
    - id: id_014__ptkathma__liquidity_baking_toggle_vote
      type: s1
  id_014__ptkathma__mutez:
    seq:
    - id: id_014__ptkathma__mutez
      type: n
  id_014__ptkathma__operation__alpha__contents:
    seq:
    - id: id_014__ptkathma__operation__alpha__contents_tag
      type: u1
      enum: id_014__ptkathma__operation__alpha__contents_tag
    - id: endorsement
      type: endorsement
      if: (id_014__ptkathma__operation__alpha__contents_tag == id_014__ptkathma__operation__alpha__contents_tag::endorsement)
    - id: preendorsement
      type: preendorsement
      if: (id_014__ptkathma__operation__alpha__contents_tag == id_014__ptkathma__operation__alpha__contents_tag::preendorsement)
    - id: dal_slot_availability
      type: dal_slot_availability
      if: (id_014__ptkathma__operation__alpha__contents_tag == id_014__ptkathma__operation__alpha__contents_tag::dal_slot_availability)
    - id: seed_nonce_revelation
      type: seed_nonce_revelation
      if: (id_014__ptkathma__operation__alpha__contents_tag == id_014__ptkathma__operation__alpha__contents_tag::seed_nonce_revelation)
    - id: vdf_revelation
      type: solution
      if: (id_014__ptkathma__operation__alpha__contents_tag == id_014__ptkathma__operation__alpha__contents_tag::vdf_revelation)
    - id: double_endorsement_evidence
      type: double_endorsement_evidence
      if: (id_014__ptkathma__operation__alpha__contents_tag == id_014__ptkathma__operation__alpha__contents_tag::double_endorsement_evidence)
    - id: double_preendorsement_evidence
      type: double_preendorsement_evidence
      if: (id_014__ptkathma__operation__alpha__contents_tag == id_014__ptkathma__operation__alpha__contents_tag::double_preendorsement_evidence)
    - id: double_baking_evidence
      type: double_baking_evidence
      if: (id_014__ptkathma__operation__alpha__contents_tag == id_014__ptkathma__operation__alpha__contents_tag::double_baking_evidence)
    - id: activate_account
      type: activate_account
      if: (id_014__ptkathma__operation__alpha__contents_tag == id_014__ptkathma__operation__alpha__contents_tag::activate_account)
    - id: proposals
      type: proposals_1
      if: (id_014__ptkathma__operation__alpha__contents_tag == id_014__ptkathma__operation__alpha__contents_tag::proposals)
    - id: ballot
      type: ballot
      if: (id_014__ptkathma__operation__alpha__contents_tag == id_014__ptkathma__operation__alpha__contents_tag::ballot)
    - id: reveal
      type: reveal
      if: (id_014__ptkathma__operation__alpha__contents_tag == id_014__ptkathma__operation__alpha__contents_tag::reveal)
    - id: transaction
      type: transaction
      if: (id_014__ptkathma__operation__alpha__contents_tag == id_014__ptkathma__operation__alpha__contents_tag::transaction)
    - id: origination
      type: origination
      if: (id_014__ptkathma__operation__alpha__contents_tag == id_014__ptkathma__operation__alpha__contents_tag::origination)
    - id: delegation
      type: delegation
      if: (id_014__ptkathma__operation__alpha__contents_tag == id_014__ptkathma__operation__alpha__contents_tag::delegation)
    - id: set_deposits_limit
      type: set_deposits_limit
      if: (id_014__ptkathma__operation__alpha__contents_tag == id_014__ptkathma__operation__alpha__contents_tag::set_deposits_limit)
    - id: increase_paid_storage
      type: increase_paid_storage
      if: (id_014__ptkathma__operation__alpha__contents_tag == id_014__ptkathma__operation__alpha__contents_tag::increase_paid_storage)
    - id: failing_noop
      type: bytes_dyn_uint30
      if: (id_014__ptkathma__operation__alpha__contents_tag == id_014__ptkathma__operation__alpha__contents_tag::failing_noop)
    - id: register_global_constant
      type: register_global_constant
      if: (id_014__ptkathma__operation__alpha__contents_tag == id_014__ptkathma__operation__alpha__contents_tag::register_global_constant)
    - id: tx_rollup_origination
      type: tx_rollup_origination
      if: (id_014__ptkathma__operation__alpha__contents_tag == id_014__ptkathma__operation__alpha__contents_tag::tx_rollup_origination)
    - id: tx_rollup_submit_batch
      type: tx_rollup_submit_batch
      if: (id_014__ptkathma__operation__alpha__contents_tag == id_014__ptkathma__operation__alpha__contents_tag::tx_rollup_submit_batch)
    - id: tx_rollup_commit
      type: tx_rollup_commit
      if: (id_014__ptkathma__operation__alpha__contents_tag == id_014__ptkathma__operation__alpha__contents_tag::tx_rollup_commit)
    - id: tx_rollup_return_bond
      type: tx_rollup_return_bond
      if: (id_014__ptkathma__operation__alpha__contents_tag == id_014__ptkathma__operation__alpha__contents_tag::tx_rollup_return_bond)
    - id: tx_rollup_finalize_commitment
      type: tx_rollup_finalize_commitment
      if: (id_014__ptkathma__operation__alpha__contents_tag == id_014__ptkathma__operation__alpha__contents_tag::tx_rollup_finalize_commitment)
    - id: tx_rollup_remove_commitment
      type: tx_rollup_remove_commitment
      if: (id_014__ptkathma__operation__alpha__contents_tag == id_014__ptkathma__operation__alpha__contents_tag::tx_rollup_remove_commitment)
    - id: tx_rollup_rejection
      type: tx_rollup_rejection
      if: (id_014__ptkathma__operation__alpha__contents_tag == id_014__ptkathma__operation__alpha__contents_tag::tx_rollup_rejection)
    - id: tx_rollup_dispatch_tickets
      type: tx_rollup_dispatch_tickets
      if: (id_014__ptkathma__operation__alpha__contents_tag == id_014__ptkathma__operation__alpha__contents_tag::tx_rollup_dispatch_tickets)
    - id: transfer_ticket
      type: transfer_ticket
      if: (id_014__ptkathma__operation__alpha__contents_tag == id_014__ptkathma__operation__alpha__contents_tag::transfer_ticket)
    - id: dal_publish_slot_header
      type: dal_publish_slot_header
      if: (id_014__ptkathma__operation__alpha__contents_tag == id_014__ptkathma__operation__alpha__contents_tag::dal_publish_slot_header)
    - id: sc_rollup_originate
      type: sc_rollup_originate
      if: (id_014__ptkathma__operation__alpha__contents_tag == id_014__ptkathma__operation__alpha__contents_tag::sc_rollup_originate)
    - id: sc_rollup_add_messages
      type: sc_rollup_add_messages
      if: (id_014__ptkathma__operation__alpha__contents_tag == id_014__ptkathma__operation__alpha__contents_tag::sc_rollup_add_messages)
    - id: sc_rollup_cement
      type: sc_rollup_cement
      if: (id_014__ptkathma__operation__alpha__contents_tag == id_014__ptkathma__operation__alpha__contents_tag::sc_rollup_cement)
    - id: sc_rollup_publish
      type: sc_rollup_publish
      if: (id_014__ptkathma__operation__alpha__contents_tag == id_014__ptkathma__operation__alpha__contents_tag::sc_rollup_publish)
    - id: sc_rollup_refute
      type: sc_rollup_refute
      if: (id_014__ptkathma__operation__alpha__contents_tag == id_014__ptkathma__operation__alpha__contents_tag::sc_rollup_refute)
    - id: sc_rollup_timeout
      type: sc_rollup_timeout
      if: (id_014__ptkathma__operation__alpha__contents_tag == id_014__ptkathma__operation__alpha__contents_tag::sc_rollup_timeout)
    - id: sc_rollup_execute_outbox_message
      type: sc_rollup_execute_outbox_message
      if: (id_014__ptkathma__operation__alpha__contents_tag == id_014__ptkathma__operation__alpha__contents_tag::sc_rollup_execute_outbox_message)
    - id: sc_rollup_recover_bond
      type: sc_rollup_recover_bond
      if: (id_014__ptkathma__operation__alpha__contents_tag == id_014__ptkathma__operation__alpha__contents_tag::sc_rollup_recover_bond)
    - id: sc_rollup_dal_slot_subscribe
      type: sc_rollup_dal_slot_subscribe
      if: (id_014__ptkathma__operation__alpha__contents_tag == id_014__ptkathma__operation__alpha__contents_tag::sc_rollup_dal_slot_subscribe)
  id_014__ptkathma__rollup_address:
    seq:
    - id: id_014__ptkathma__rollup_address
      type: bytes_dyn_uint30
  id_014__ptkathma__scripted__contracts:
    seq:
    - id: code
      type: bytes_dyn_uint30
    - id: storage
      type: bytes_dyn_uint30
  id_014__ptkathma__tx_rollup_id:
    seq:
    - id: rollup_hash
      size: 20
  inbox:
    seq:
    - id: inbox_tag
      type: u1
      enum: inbox_tag
    - id: some
      type: some_0
      if: (inbox_tag == inbox_tag::some)
  inc:
    seq:
    - id: inc_entries
      type: inc_entries
      repeat: eos
  inc_0:
    seq:
    - id: len_inc
      type: u4be
      valid:
        max: 1073741823
    - id: inc
      type: inc
      size: len_inc
  inc_entries:
    seq:
    - id: index
      type: int31
    - id: content
      size: 32
    - id: back_pointers
      type: back_pointers_0
  increase_paid_storage:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_014__ptkathma__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: amount
      type: z
    - id: destination
      type: id_014__ptkathma__contract_id__originated
      doc: ! >-
        A contract handle -- originated account: A contract notation as given to an
        RPC or inside scripts. Can be a base58 originated contract hash.
  inode:
    seq:
    - id: inode_field0
      type: u1
    - id: inode_field1
      size: 32
      doc: ! 'context_hash


        inode_field1_field1'
  inode_0:
    seq:
    - id: inode_field0
      type: u1
    - id: inode_field1
      size: 32
      doc: ! 'context_hash


        inode_field1_field0'
  inode_1:
    seq:
    - id: inode_field0
      type: u1
    - id: inode_field1
      type: inode_field1
  inode_10:
    seq:
    - id: inode_field0
      type: s8be
    - id: inode_field1
      type: inode_field1
  inode_11:
    seq:
    - id: length
      type: s8be
    - id: proofs
      type: proofs_0
  inode_2:
    seq:
    - id: inode_field0
      type: u2be
    - id: inode_field1
      size: 32
      doc: ! 'context_hash


        inode_field1_field1'
  inode_3:
    seq:
    - id: inode_field0
      type: u2be
    - id: inode_field1
      size: 32
      doc: ! 'context_hash


        inode_field1_field0'
  inode_4:
    seq:
    - id: inode_field0
      type: u2be
    - id: inode_field1
      type: inode_field1
  inode_5:
    seq:
    - id: inode_field0
      type: s4be
    - id: inode_field1
      size: 32
      doc: ! 'context_hash


        inode_field1_field1'
  inode_6:
    seq:
    - id: inode_field0
      type: s4be
    - id: inode_field1
      size: 32
      doc: ! 'context_hash


        inode_field1_field0'
  inode_7:
    seq:
    - id: inode_field0
      type: s4be
    - id: inode_field1
      type: inode_field1
  inode_8:
    seq:
    - id: inode_field0
      type: s8be
    - id: inode_field1
      size: 32
      doc: ! 'context_hash


        inode_field1_field1'
  inode_9:
    seq:
    - id: inode_field0
      type: s8be
    - id: inode_field1
      size: 32
      doc: ! 'context_hash


        inode_field1_field0'
  inode_extender:
    seq:
    - id: length
      type: s8be
    - id: segment
      type: segment_0
    - id: proof
      type: inode_tree
  inode_field1:
    seq:
    - id: inode_field1_field0
      size: 32
      doc: context_hash
    - id: inode_field1_field1
      size: 32
      doc: context_hash
  inode_tree:
    seq:
    - id: length
      type: s8be
    - id: proofs
      type: proofs
  inode_tree_0:
    seq:
    - id: inode_tree_tag
      type: u1
      enum: inode_tree_tag
    - id: blinded_inode
      size: 32
      if: (inode_tree_tag == inode_tree_tag::blinded_inode)
    - id: inode_values
      type: inode_values_0
      if: (inode_tree_tag == inode_tree_tag::inode_values)
    - id: inode_tree
      type: inode_tree
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: inode_extender
      type: inode_extender
      if: (inode_tree_tag == inode_tree_tag::inode_extender)
  inode_values:
    seq:
    - id: inode_values_entries
      type: inode_values_entries
      repeat: eos
  inode_values_0:
    seq:
    - id: len_inode_values
      type: u4be
      valid:
        max: 1073741823
    - id: inode_values
      type: inode_values
      size: len_inode_values
  inode_values_elt_field0:
    seq:
    - id: inode_values_elt_field0
      size-eos: true
  inode_values_elt_field0_0:
    seq:
    - id: len_inode_values_elt_field0
      type: u1
      valid:
        max: 255
    - id: inode_values_elt_field0
      type: inode_values_elt_field0
      size: len_inode_values_elt_field0
  inode_values_entries:
    seq:
    - id: inode_values_elt_field0
      type: inode_values_elt_field0_0
    - id: inode_values_elt_field1
      type: tree_encoding
  int31:
    seq:
    - id: int31
      type: s4be
      valid:
        min: -1073741824
        max: 1073741823
  level:
    seq:
    - id: rollup
      type: id_014__ptkathma__rollup_address
      doc: ! >-
        A smart contract rollup address: A smart contract rollup is identified by
        a base58 address starting with scr1
    - id: message_counter
      type: n
    - id: nb_available_messages
      type: s8be
    - id: nb_messages_in_commitment_period
      type: s8be
    - id: starting_level_of_current_commitment_period
      type: s4be
    - id: level
      type: s4be
    - id: current_messages_hash
      size: 32
    - id: old_levels_messages
      type: old_levels_messages
  message:
    seq:
    - id: message_tag
      type: u1
      enum: message_tag
    - id: batch
      type: bytes_dyn_uint30
      if: (message_tag == message_tag::batch)
    - id: deposit
      type: deposit
      if: (message_tag == message_tag::deposit)
  message_0:
    seq:
    - id: message_entries
      type: message_entries
      repeat: eos
  message_1:
    seq:
    - id: len_message
      type: u4be
      valid:
        max: 1073741823
    - id: message
      type: message_0
      size: len_message
  message_entries:
    seq:
    - id: message_elt
      type: bytes_dyn_uint30
  message_path:
    seq:
    - id: message_path_entries
      type: message_path_entries
      repeat: eos
  message_path_0:
    seq:
    - id: len_message_path
      type: u4be
      valid:
        max: 1073741823
    - id: message_path
      type: message_path
      size: len_message_path
  message_path_entries:
    seq:
    - id: inbox_list_hash
      size: 32
  message_proof:
    seq:
    - id: version
      type: s2be
    - id: before
      type: before
    - id: after
      type: after
    - id: state
      type: tree_encoding
  message_result_path:
    seq:
    - id: message_result_path_entries
      type: message_result_path_entries
      repeat: eos
  message_result_path_0:
    seq:
    - id: len_message_result_path
      type: u4be
      valid:
        max: 1073741823
    - id: message_result_path
      type: message_result_path
      size: len_message_result_path
  message_result_path_entries:
    seq:
    - id: message_result_list_hash
      size: 32
  messages:
    seq:
    - id: messages_entries
      type: messages_entries
      repeat: eos
  messages_0:
    seq:
    - id: len_messages
      type: u4be
      valid:
        max: 1073741823
    - id: messages
      type: messages
      size: len_messages
  messages_entries:
    seq:
    - id: message_result_hash
      size: 32
  n:
    seq:
    - id: n
      type: n_chunk
      repeat: until
      repeat-until: not (_.has_more).as<bool>
  n_chunk:
    seq:
    - id: has_more
      type: b1be
    - id: payload
      type: b7be
  named:
    seq:
    - id: named
      size-eos: true
  named_0:
    seq:
    - id: len_named
      type: u1
      valid:
        max: 31
    - id: named
      type: named
      size: len_named
  node:
    seq:
    - id: node_entries
      type: node_entries
      repeat: eos
  node_0:
    seq:
    - id: len_node
      type: u4be
      valid:
        max: 1073741823
    - id: node
      type: node
      size: len_node
  node_elt_field0:
    seq:
    - id: node_elt_field0
      size-eos: true
  node_elt_field0_0:
    seq:
    - id: len_node_elt_field0
      type: u1
      valid:
        max: 255
    - id: node_elt_field0
      type: node_elt_field0
      size: len_node_elt_field0
  node_entries:
    seq:
    - id: node_elt_field0
      type: node_elt_field0_0
    - id: node_elt_field1
      type: tree_encoding
  old_levels_messages:
    seq:
    - id: index
      type: int31
    - id: content
      size: 32
    - id: back_pointers
      type: back_pointers_0
  op1:
    seq:
    - id: id_014__ptkathma__inlined__endorsement
      type: id_014__ptkathma__inlined__endorsement
  op1_0:
    seq:
    - id: len_op1
      type: u4be
      valid:
        max: 1073741823
    - id: op1
      type: op1
      size: len_op1
  op1_1:
    seq:
    - id: id_014__ptkathma__inlined__preendorsement
      type: id_014__ptkathma__inlined__preendorsement
  op1_2:
    seq:
    - id: len_op1
      type: u4be
      valid:
        max: 1073741823
    - id: op1
      type: op1_1
      size: len_op1
  op2:
    seq:
    - id: id_014__ptkathma__inlined__endorsement
      type: id_014__ptkathma__inlined__endorsement
  op2_0:
    seq:
    - id: len_op2
      type: u4be
      valid:
        max: 1073741823
    - id: op2
      type: op2
      size: len_op2
  op2_1:
    seq:
    - id: id_014__ptkathma__inlined__preendorsement
      type: id_014__ptkathma__inlined__preendorsement
  op2_2:
    seq:
    - id: len_op2
      type: u4be
      valid:
        max: 1073741823
    - id: op2
      type: op2_1
      size: len_op2
  originated:
    seq:
    - id: contract_hash
      size: 20
    - id: originated_padding
      size: 1
      doc: This field is for padding, ignore
  origination:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_014__ptkathma__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: balance
      type: id_014__ptkathma__mutez
    - id: delegate_tag
      type: u1
      enum: bool
    - id: delegate
      type: public_key_hash
      if: (delegate_tag == bool::true)
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: script
      type: id_014__ptkathma__scripted__contracts
  other_elts:
    seq:
    - id: other_elts_entries
      type: other_elts_entries
      repeat: eos
  other_elts_0:
    seq:
    - id: len_other_elts
      type: u4be
      valid:
        max: 1073741823
    - id: other_elts
      type: other_elts
      size: len_other_elts
  other_elts_1:
    seq:
    - id: other_elts
      size-eos: true
  other_elts_2:
    seq:
    - id: len_other_elts
      type: u1
      valid:
        max: 255
    - id: other_elts
      type: other_elts_1
      size: len_other_elts
  other_elts_3:
    seq:
    - id: len_other_elts
      type: u2be
      valid:
        max: 65535
    - id: other_elts
      type: other_elts_1
      size: len_other_elts
  other_elts_4:
    seq:
    - id: other_elts_field0
      type: u1
    - id: other_elts_field1
      type: other_elts_field1_0
    - id: other_elts_field2
      size: 32
      doc: context_hash
  other_elts_5:
    seq:
    - id: other_elts_field0
      type: u2be
    - id: other_elts_field1
      type: other_elts_field1_0
    - id: other_elts_field2
      size: 32
      doc: context_hash
  other_elts_6:
    seq:
    - id: other_elts_field0
      type: s4be
    - id: other_elts_field1
      type: other_elts_field1_0
    - id: other_elts_field2
      size: 32
      doc: context_hash
  other_elts_7:
    seq:
    - id: other_elts_field0
      type: s8be
    - id: other_elts_field1
      type: other_elts_field1_0
    - id: other_elts_field2
      size: 32
      doc: context_hash
  other_elts_elt_field0:
    seq:
    - id: other_elts_elt_field0
      size-eos: true
  other_elts_elt_field0_0:
    seq:
    - id: len_other_elts_elt_field0
      type: u1
      valid:
        max: 255
    - id: other_elts_elt_field0
      type: other_elts_elt_field0
      size: len_other_elts_elt_field0
  other_elts_elt_field1:
    seq:
    - id: other_elts_elt_field1_tag
      type: u1
      enum: other_elts_elt_field1_tag
    - id: value
      size: 32
      if: (other_elts_elt_field1_tag == other_elts_elt_field1_tag::value)
    - id: node
      size: 32
      if: (other_elts_elt_field1_tag == other_elts_elt_field1_tag::node)
  other_elts_entries:
    seq:
    - id: other_elts_elt_field0
      type: other_elts_elt_field0_0
    - id: other_elts_elt_field1
      type: other_elts_elt_field1
  other_elts_field1:
    seq:
    - id: other_elts_field1
      size-eos: true
  other_elts_field1_0:
    seq:
    - id: len_other_elts_field1
      type: u1
      valid:
        max: 255
    - id: other_elts_field1
      type: other_elts_field1
      size: len_other_elts_field1
  parameters:
    seq:
    - id: entrypoint
      type: id_014__ptkathma__entrypoint
      doc: ! 'entrypoint: Named entrypoint to a Michelson smart contract'
    - id: value
      type: bytes_dyn_uint30
  predecessor:
    seq:
    - id: predecessor_tag
      type: u1
      enum: predecessor_tag
    - id: some
      size: 32
      if: (predecessor_tag == predecessor_tag::some)
  preendorsement:
    seq:
    - id: slot
      type: u2be
    - id: level
      type: s4be
    - id: round
      type: s4be
    - id: block_payload_hash
      size: 32
  previous_message_result:
    seq:
    - id: context_hash
      size: 32
    - id: withdraw_list_hash
      size: 32
  previous_message_result_path:
    seq:
    - id: previous_message_result_path_entries
      type: previous_message_result_path_entries
      repeat: eos
  previous_message_result_path_0:
    seq:
    - id: len_previous_message_result_path
      type: u4be
      valid:
        max: 1073741823
    - id: previous_message_result_path
      type: previous_message_result_path
      size: len_previous_message_result_path
  previous_message_result_path_entries:
    seq:
    - id: message_result_list_hash
      size: 32
  proof:
    seq:
    - id: proof_tag
      type: u1
      enum: proof_tag
    - id: case_0
      type: case_0
      if: (proof_tag == proof_tag::case_0)
    - id: case_2
      type: case_2
      if: (proof_tag == proof_tag::case_2)
    - id: case_1
      type: case_1
      if: (proof_tag == proof_tag::case_1)
    - id: case_3
      type: case_3
      if: (proof_tag == proof_tag::case_3)
  proof_0:
    seq:
    - id: pvm_step
      type: pvm_step
    - id: inbox
      type: inbox
  proofs:
    seq:
    - id: proofs_tag
      type: u1
      enum: proofs_tag
    - id: sparse_proof
      type: sparse_proof_0
      if: (proofs_tag == proofs_tag::sparse_proof)
    - id: dense_proof
      type: dense_proof_entries
      if: (proofs_tag == proofs_tag::dense_proof)
  proofs_0:
    seq:
    - id: proofs_tag
      type: u1
      enum: proofs_tag
    - id: sparse_proof
      type: sparse_proof_2
      if: (proofs_tag == proofs_tag::sparse_proof)
    - id: dense_proof
      type: dense_proof_entries
      if: (proofs_tag == proofs_tag::dense_proof)
  proposals:
    seq:
    - id: proposals_entries
      type: proposals_entries
      repeat: eos
  proposals_0:
    seq:
    - id: len_proposals
      type: u4be
      valid:
        max: 1073741823
    - id: proposals
      type: proposals
      size: len_proposals
  proposals_1:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: period
      type: s4be
    - id: proposals
      type: proposals_0
  proposals_entries:
    seq:
    - id: protocol_hash
      size: 32
  public_key:
    seq:
    - id: public_key_tag
      type: u1
      enum: public_key_tag
    - id: ed25519
      size: 32
      if: (public_key_tag == public_key_tag::ed25519)
    - id: secp256k1
      size: 33
      if: (public_key_tag == public_key_tag::secp256k1)
    - id: p256
      size: 33
      if: (public_key_tag == public_key_tag::p256)
  public_key_hash:
    seq:
    - id: public_key_hash_tag
      type: u1
      enum: public_key_hash_tag
    - id: ed25519
      size: 20
      if: (public_key_hash_tag == public_key_hash_tag::ed25519)
    - id: secp256k1
      size: 20
      if: (public_key_hash_tag == public_key_hash_tag::secp256k1)
    - id: p256
      size: 20
      if: (public_key_hash_tag == public_key_hash_tag::p256)
  pvm_step:
    seq:
    - id: pvm_step_tag
      type: u1
      enum: pvm_step_tag
    - id: arithmetic__pvm__with__proof
      type: arithmetic__pvm__with__proof
      if: (pvm_step_tag == pvm_step_tag::arithmetic__pvm__with__proof)
    - id: wasm__2__0__0__pvm__with__proof
      type: wasm__2__0__0__pvm__with__proof
      if: (pvm_step_tag == pvm_step_tag::wasm__2__0__0__pvm__with__proof)
  refutation:
    seq:
    - id: choice
      type: n
    - id: step
      type: step
  register_global_constant:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_014__ptkathma__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: value
      type: bytes_dyn_uint30
  requested:
    seq:
    - id: requested_tag
      type: u1
      enum: requested_tag
    - id: first_after
      type: first_after
      if: (requested_tag == requested_tag::first_after)
  reveal:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_014__ptkathma__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: public_key
      type: public_key
      doc: A Ed25519, Secp256k1, or P256 public key
  sc_rollup_add_messages:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_014__ptkathma__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      type: id_014__ptkathma__rollup_address
      doc: ! >-
        A smart contract rollup address: A smart contract rollup is identified by
        a base58 address starting with scr1
    - id: message
      type: message_1
  sc_rollup_cement:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_014__ptkathma__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      type: id_014__ptkathma__rollup_address
      doc: ! >-
        A smart contract rollup address: A smart contract rollup is identified by
        a base58 address starting with scr1
    - id: commitment
      size: 32
  sc_rollup_dal_slot_subscribe:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_014__ptkathma__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      type: id_014__ptkathma__rollup_address
      doc: ! >-
        A smart contract rollup address: A smart contract rollup is identified by
        a base58 address starting with scr1
    - id: slot_index
      type: u1
  sc_rollup_execute_outbox_message:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_014__ptkathma__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      type: id_014__ptkathma__rollup_address
      doc: ! >-
        A smart contract rollup address: A smart contract rollup is identified by
        a base58 address starting with scr1
    - id: cemented_commitment
      size: 32
    - id: outbox_level
      type: s4be
    - id: message_index
      type: int31
    - id: inclusion__proof
      type: bytes_dyn_uint30
    - id: message
      type: bytes_dyn_uint30
  sc_rollup_originate:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_014__ptkathma__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: kind
      type: u2
      enum: kind_tag
    - id: boot_sector
      type: bytes_dyn_uint30
    - id: parameters_ty
      type: bytes_dyn_uint30
  sc_rollup_publish:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_014__ptkathma__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      type: id_014__ptkathma__rollup_address
      doc: ! >-
        A smart contract rollup address: A smart contract rollup is identified by
        a base58 address starting with scr1
    - id: commitment
      type: commitment_0
  sc_rollup_recover_bond:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_014__ptkathma__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      size: 20
  sc_rollup_refute:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_014__ptkathma__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      type: id_014__ptkathma__rollup_address
      doc: ! >-
        A smart contract rollup address: A smart contract rollup is identified by
        a base58 address starting with scr1
    - id: opponent
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: refutation
      type: refutation
    - id: is_opening_move
      type: u1
      enum: bool
  sc_rollup_timeout:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_014__ptkathma__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      type: id_014__ptkathma__rollup_address
      doc: ! >-
        A smart contract rollup address: A smart contract rollup is identified by
        a base58 address starting with scr1
    - id: stakers
      type: stakers
  seed_nonce_revelation:
    seq:
    - id: level
      type: s4be
    - id: nonce
      size: 32
  segment:
    seq:
    - id: segment
      size-eos: true
  segment_0:
    seq:
    - id: len_segment
      type: u1
      valid:
        max: 255
    - id: segment
      type: segment
      size: len_segment
  set_deposits_limit:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_014__ptkathma__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: limit_tag
      type: u1
      enum: bool
    - id: limit
      type: id_014__ptkathma__mutez
      if: (limit_tag == bool::true)
  skips:
    seq:
    - id: skips_entries
      type: skips_entries
      repeat: eos
  skips_0:
    seq:
    - id: len_skips
      type: u4be
      valid:
        max: 1073741823
    - id: skips
      type: skips
      size: len_skips
  skips_elt_field0:
    seq:
    - id: rollup
      type: id_014__ptkathma__rollup_address
      doc: ! >-
        A smart contract rollup address: A smart contract rollup is identified by
        a base58 address starting with scr1
    - id: message_counter
      type: n
    - id: nb_available_messages
      type: s8be
    - id: nb_messages_in_commitment_period
      type: s8be
    - id: starting_level_of_current_commitment_period
      type: s4be
    - id: level
      type: s4be
    - id: current_messages_hash
      size: 32
    - id: old_levels_messages
      type: old_levels_messages
  skips_elt_field1:
    seq:
    - id: skips_elt_field1_entries
      type: skips_elt_field1_entries
      repeat: eos
  skips_elt_field1_0:
    seq:
    - id: len_skips_elt_field1
      type: u4be
      valid:
        max: 1073741823
    - id: skips_elt_field1
      type: skips_elt_field1
      size: len_skips_elt_field1
  skips_elt_field1_entries:
    seq:
    - id: index
      type: int31
    - id: content
      size: 32
    - id: back_pointers
      type: back_pointers_0
  skips_entries:
    seq:
    - id: skips_elt_field0
      type: skips_elt_field0
    - id: skips_elt_field1
      type: skips_elt_field1_0
  slot:
    seq:
    - id: level
      type: s4be
    - id: index
      type: u1
    - id: header
      type: int31
  solution:
    seq:
    - id: solution_field0
      size: 100
    - id: solution_field1
      size: 100
  some:
    seq:
    - id: inbox_level
      type: s4be
    - id: message_counter
      type: n
    - id: payload
      type: bytes_dyn_uint30
  some_0:
    seq:
    - id: skips
      type: skips_0
    - id: level
      type: level
    - id: inc
      type: inc_0
    - id: message_proof
      type: message_proof
  sparse_proof:
    seq:
    - id: sparse_proof_entries
      type: sparse_proof_entries
      repeat: eos
  sparse_proof_0:
    seq:
    - id: len_sparse_proof
      type: u4be
      valid:
        max: 1073741823
    - id: sparse_proof
      type: sparse_proof
      size: len_sparse_proof
  sparse_proof_1:
    seq:
    - id: sparse_proof_entries
      type: sparse_proof_entries_0
      repeat: eos
  sparse_proof_2:
    seq:
    - id: len_sparse_proof
      type: u4be
      valid:
        max: 1073741823
    - id: sparse_proof
      type: sparse_proof_1
      size: len_sparse_proof
  sparse_proof_entries:
    seq:
    - id: sparse_proof_elt_field0
      type: u1
    - id: sparse_proof_elt_field1
      type: inode_tree
  sparse_proof_entries_0:
    seq:
    - id: sparse_proof_elt_field0
      type: u1
    - id: sparse_proof_elt_field1
      type: inode_tree_0
      doc: inode_tree
  stakers:
    seq:
    - id: alice
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: bob
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
  step:
    seq:
    - id: step_tag
      type: u1
      enum: step_tag
    - id: dissection
      type: dissection_0
      if: (step_tag == step_tag::dissection)
    - id: proof
      type: proof_0
      if: (step_tag == step_tag::proof)
  tickets_info:
    seq:
    - id: tickets_info_entries
      type: tickets_info_entries
      repeat: eos
  tickets_info_0:
    seq:
    - id: len_tickets_info
      type: u4be
      valid:
        max: 1073741823
    - id: tickets_info
      type: tickets_info
      size: len_tickets_info
  tickets_info_entries:
    seq:
    - id: contents
      type: bytes_dyn_uint30
    - id: ty
      type: bytes_dyn_uint30
    - id: ticketer
      type: id_014__ptkathma__contract_id
      doc: ! >-
        A contract handle: A contract notation as given to an RPC or inside scripts.
        Can be a base58 implicit contract hash or a base58 originated contract hash.
    - id: amount
      type: amount
    - id: claimer
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
  transaction:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_014__ptkathma__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: amount
      type: id_014__ptkathma__mutez
    - id: destination
      type: id_014__ptkathma__contract_id
      doc: ! >-
        A contract handle: A contract notation as given to an RPC or inside scripts.
        Can be a base58 implicit contract hash or a base58 originated contract hash.
    - id: parameters_tag
      type: u1
      enum: bool
    - id: parameters
      type: parameters
      if: (parameters_tag == bool::true)
  transfer_ticket:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_014__ptkathma__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: ticket_contents
      type: bytes_dyn_uint30
    - id: ticket_ty
      type: bytes_dyn_uint30
    - id: ticket_ticketer
      type: id_014__ptkathma__contract_id
      doc: ! >-
        A contract handle: A contract notation as given to an RPC or inside scripts.
        Can be a base58 implicit contract hash or a base58 originated contract hash.
    - id: ticket_amount
      type: n
    - id: destination
      type: id_014__ptkathma__contract_id
      doc: ! >-
        A contract handle: A contract notation as given to an RPC or inside scripts.
        Can be a base58 implicit contract hash or a base58 originated contract hash.
    - id: entrypoint
      type: bytes_dyn_uint30
  tree_encoding:
    seq:
    - id: tree_encoding_tag
      type: u1
      enum: tree_encoding_tag
    - id: value
      type: bytes_dyn_uint30
      if: (tree_encoding_tag == tree_encoding_tag::value)
    - id: blinded_value
      size: 32
      if: (tree_encoding_tag == tree_encoding_tag::blinded_value)
    - id: node
      type: node_0
      if: (tree_encoding_tag == tree_encoding_tag::node)
    - id: blinded_node
      size: 32
      if: (tree_encoding_tag == tree_encoding_tag::blinded_node)
    - id: inode
      type: inode_11
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: extender
      type: extender
      if: (tree_encoding_tag == tree_encoding_tag::extender)
  tree_proof:
    seq:
    - id: version
      type: s2be
    - id: before
      type: before
    - id: after
      type: after
    - id: state
      type: tree_encoding
  tx_rollup_commit:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_014__ptkathma__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      type: id_014__ptkathma__tx_rollup_id
      doc: ! >-
        A tx rollup handle: A tx rollup notation as given to an RPC or inside scripts,
        is a base58 tx rollup hash
    - id: commitment
      type: commitment
  tx_rollup_dispatch_tickets:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_014__ptkathma__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: tx_rollup
      type: id_014__ptkathma__tx_rollup_id
      doc: ! >-
        A tx rollup handle: A tx rollup notation as given to an RPC or inside scripts,
        is a base58 tx rollup hash
    - id: level
      type: s4be
    - id: context_hash
      size: 32
    - id: message_index
      type: int31
    - id: message_result_path
      type: message_result_path_0
    - id: tickets_info
      type: tickets_info_0
  tx_rollup_finalize_commitment:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_014__ptkathma__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      type: id_014__ptkathma__tx_rollup_id
      doc: ! >-
        A tx rollup handle: A tx rollup notation as given to an RPC or inside scripts,
        is a base58 tx rollup hash
  tx_rollup_origination:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_014__ptkathma__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
  tx_rollup_rejection:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_014__ptkathma__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      type: id_014__ptkathma__tx_rollup_id
      doc: ! >-
        A tx rollup handle: A tx rollup notation as given to an RPC or inside scripts,
        is a base58 tx rollup hash
    - id: level
      type: s4be
    - id: message
      type: message
    - id: message_position
      type: n
    - id: message_path
      type: message_path_0
    - id: message_result_hash
      size: 32
    - id: message_result_path
      type: message_result_path_0
    - id: previous_message_result
      type: previous_message_result
    - id: previous_message_result_path
      type: previous_message_result_path_0
    - id: proof
      type: proof
  tx_rollup_remove_commitment:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_014__ptkathma__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      type: id_014__ptkathma__tx_rollup_id
      doc: ! >-
        A tx rollup handle: A tx rollup notation as given to an RPC or inside scripts,
        is a base58 tx rollup hash
  tx_rollup_return_bond:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_014__ptkathma__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      type: id_014__ptkathma__tx_rollup_id
      doc: ! >-
        A tx rollup handle: A tx rollup notation as given to an RPC or inside scripts,
        is a base58 tx rollup hash
  tx_rollup_submit_batch:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_014__ptkathma__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      type: id_014__ptkathma__tx_rollup_id
      doc: ! >-
        A tx rollup handle: A tx rollup notation as given to an RPC or inside scripts,
        is a base58 tx rollup hash
    - id: content
      type: bytes_dyn_uint30
    - id: burn_limit_tag
      type: u1
      enum: bool
    - id: burn_limit
      type: id_014__ptkathma__mutez
      if: (burn_limit_tag == bool::true)
  wasm__2__0__0__pvm__with__proof:
    seq:
    - id: tree_proof
      type: tree_proof
    - id: given
      type: given
    - id: requested
      type: requested
  z:
    seq:
    - id: has_tail
      type: b1be
    - id: sign
      type: b1be
    - id: payload
      type: b6be
    - id: tail
      type: n_chunk
      repeat: until
      repeat-until: not (_.has_more).as<bool>
      if: has_tail.as<bool>
enums:
  after_tag:
    0: value
    1: node
  amount_tag:
    0: small
    1: medium
    2: biggish
    3: bigger
  before_tag:
    0: value
    1: node
  bool:
    0: false
    255: true
  case_0_field3_elt_tag:
    0: inode
    1: inode
    2: inode
    3: inode
    4: inode
    5: inode
    6: inode
    7: inode
    8: inode
    9: inode
    10: inode
    11: inode
    12: inode
    13: inode
    14: inode
    15: inode
    128: other_elts
    129: other_elts
    130: other_elts
    131: other_elts
    192: other_elts
    193: other_elts
    195: other_elts
    224: other_elts
    225: other_elts
    226: other_elts
    227: other_elts
  case_1_field3_elt_tag:
    0: inode
    1: inode
    2: inode
    3: inode
    4: inode
    5: inode
    6: inode
    7: inode
    8: inode
    9: inode
    10: inode
    11: inode
    12: inode
    13: inode
    14: inode
    15: inode
    128: other_elts
    129: other_elts
    130: other_elts
    131: other_elts
    192: other_elts
    193: other_elts
    195: other_elts
    224: other_elts
    225: other_elts
    226: other_elts
    227: other_elts
  case_2_field3_elt_tag:
    0: inode
    1: inode
    2: inode
    3: inode
    4: inode
    5: inode
    6: inode
    7: inode
    8: inode
    9: inode
    10: inode
    11: inode
    12: inode
    13: inode
    14: inode
    15: inode
    128: other_elts
    129: other_elts
    130: other_elts
    131: other_elts
    192: other_elts
    193: other_elts
    195: other_elts
    224: other_elts
    225: other_elts
    226: other_elts
    227: other_elts
  case_3_field3_elt_tag:
    0: inode
    1: inode
    2: inode
    3: inode
    4: inode
    5: inode
    6: inode
    7: inode
    8: inode
    9: inode
    10: inode
    11: inode
    12: inode
    13: inode
    14: inode
    15: inode
    128: other_elts
    129: other_elts
    130: other_elts
    131: other_elts
    192: other_elts
    193: other_elts
    195: other_elts
    224: other_elts
    225: other_elts
    226: other_elts
    227: other_elts
  dissection_elt_field0_tag:
    0: none
    1: some
  given_tag:
    0: none
    1: some
  id_014__ptkathma__contract_id__originated_tag:
    1: originated
  id_014__ptkathma__contract_id_tag:
    0: implicit
    1: originated
  id_014__ptkathma__entrypoint_tag:
    0: default
    1: root
    2: do
    3: set_delegate
    4: remove_delegate
    255: named
  id_014__ptkathma__inlined__endorsement_mempool__contents_tag:
    21: endorsement
  id_014__ptkathma__inlined__preendorsement__contents_tag:
    20: preendorsement
  id_014__ptkathma__operation__alpha__contents_tag:
    1: seed_nonce_revelation
    2: double_endorsement_evidence
    3: double_baking_evidence
    4: activate_account
    5: proposals
    6: ballot
    7: double_preendorsement_evidence
    8: vdf_revelation
    17: failing_noop
    20: preendorsement
    21: endorsement
    22: dal_slot_availability
    107: reveal
    108: transaction
    109: origination
    110: delegation
    111: register_global_constant
    112: set_deposits_limit
    113: increase_paid_storage
    150: tx_rollup_origination
    151: tx_rollup_submit_batch
    152: tx_rollup_commit
    153: tx_rollup_return_bond
    154: tx_rollup_finalize_commitment
    155: tx_rollup_remove_commitment
    156: tx_rollup_rejection
    157: tx_rollup_dispatch_tickets
    158: transfer_ticket
    200: sc_rollup_originate
    201: sc_rollup_add_messages
    202: sc_rollup_cement
    203: sc_rollup_publish
    204: sc_rollup_refute
    205: sc_rollup_timeout
    206: sc_rollup_execute_outbox_message
    207: sc_rollup_recover_bond
    208: sc_rollup_dal_slot_subscribe
    230: dal_publish_slot_header
  inbox_tag:
    0: none
    1: some
  inode_tree_tag:
    0: blinded_inode
    1: inode_values
    2: inode_tree
    3: inode_extender
    4: none
  kind_tag:
    0: example_arith__smart__contract__rollup__kind
    1: wasm__2__0__0__smart__contract__rollup__kind
  message_tag:
    0: batch
    1: deposit
  other_elts_elt_field1_tag:
    0: value
    1: node
  predecessor_tag:
    0: none
    1: some
  proof_tag:
    0: case_0
    1: case_1
    2: case_2
    3: case_3
  proofs_tag:
    0: sparse_proof
    1: dense_proof
  public_key_hash_tag:
    0: ed25519
    1: secp256k1
    2: p256
  public_key_tag:
    0: ed25519
    1: secp256k1
    2: p256
  pvm_step_tag:
    0: arithmetic__pvm__with__proof
    1: wasm__2__0__0__pvm__with__proof
  requested_tag:
    0: no_input_required
    1: initial
    2: first_after
  step_tag:
    0: dissection
    1: proof
  tree_encoding_tag:
    0: value
    1: blinded_value
    2: node
    3: blinded_node
    4: inode
    5: extender
    6: none
seq:
- id: id_014__ptkathma__operation__alpha__contents
  type: id_014__ptkathma__operation__alpha__contents
