meta:
  id: id_008__ptedo2zk__constants__parametric
  endian: be
doc: ! 'Encoding id: 008-PtEdo2Zk.constants.parametric'
types:
  baking_reward_per_endorsement:
    seq:
    - id: baking_reward_per_endorsement_entries
      type: baking_reward_per_endorsement_entries
      repeat: eos
  baking_reward_per_endorsement_0:
    seq:
    - id: len_baking_reward_per_endorsement
      type: u4be
      valid:
        max: 1073741823
    - id: baking_reward_per_endorsement
      type: baking_reward_per_endorsement
      size: len_baking_reward_per_endorsement
  baking_reward_per_endorsement_entries:
    seq:
    - id: id_008__ptedo2zk__mutez
      type: id_008__ptedo2zk__mutez
  endorsement_reward:
    seq:
    - id: endorsement_reward_entries
      type: endorsement_reward_entries
      repeat: eos
  endorsement_reward_0:
    seq:
    - id: len_endorsement_reward
      type: u4be
      valid:
        max: 1073741823
    - id: endorsement_reward
      type: endorsement_reward
      size: len_endorsement_reward
  endorsement_reward_entries:
    seq:
    - id: id_008__ptedo2zk__mutez
      type: id_008__ptedo2zk__mutez
  id_008__ptedo2zk__mutez:
    seq:
    - id: id_008__ptedo2zk__mutez
      type: n
  int31:
    seq:
    - id: int31
      type: s4be
      valid:
        min: -1073741824
        max: 1073741823
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
  time_between_blocks:
    seq:
    - id: time_between_blocks_entries
      type: time_between_blocks_entries
      repeat: eos
  time_between_blocks_0:
    seq:
    - id: len_time_between_blocks
      type: u4be
      valid:
        max: 1073741823
    - id: time_between_blocks
      type: time_between_blocks
      size: len_time_between_blocks
  time_between_blocks_entries:
    seq:
    - id: time_between_blocks_elt
      type: s8be
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
seq:
- id: preserved_cycles
  type: u1
- id: blocks_per_cycle
  type: s4be
- id: blocks_per_commitment
  type: s4be
- id: blocks_per_roll_snapshot
  type: s4be
- id: blocks_per_voting_period
  type: s4be
- id: time_between_blocks
  type: time_between_blocks_0
- id: endorsers_per_block
  type: u2be
- id: hard_gas_limit_per_operation
  type: z
- id: hard_gas_limit_per_block
  type: z
- id: proof_of_work_threshold
  type: s8be
- id: tokens_per_roll
  type: id_008__ptedo2zk__mutez
- id: michelson_maximum_type_size
  type: u2be
- id: seed_nonce_revelation_tip
  type: id_008__ptedo2zk__mutez
- id: origination_size
  type: int31
- id: block_security_deposit
  type: id_008__ptedo2zk__mutez
- id: endorsement_security_deposit
  type: id_008__ptedo2zk__mutez
- id: baking_reward_per_endorsement
  type: baking_reward_per_endorsement_0
- id: endorsement_reward
  type: endorsement_reward_0
- id: cost_per_byte
  type: id_008__ptedo2zk__mutez
- id: hard_storage_limit_per_operation
  type: z
- id: test_chain_duration
  type: s8be
- id: quorum_min
  type: s4be
- id: quorum_max
  type: s4be
- id: min_proposal_quorum
  type: s4be
- id: initial_endorsers
  type: u2be
- id: delay_per_missing_endorsement
  type: s8be
