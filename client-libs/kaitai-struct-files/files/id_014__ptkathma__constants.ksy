meta:
  id: id_014__ptkathma__constants
  endian: be
doc: ! 'Encoding id: 014-PtKathma.constants'
types:
  dal_parametric:
    seq:
    - id: feature_enable
      type: u1
      enum: bool
    - id: number_of_slots
      type: s2be
    - id: number_of_shards
      type: s2be
    - id: endorsement_lag
      type: s2be
    - id: availability_threshold
      type: s2be
  id_014__ptkathma__mutez:
    seq:
    - id: id_014__ptkathma__mutez
      type: n
  int31:
    seq:
    - id: int31
      type: s4be
      valid:
        min: -1073741824
        max: 1073741823
  minimal_participation_ratio:
    seq:
    - id: numerator
      type: u2be
    - id: denominator
      type: u2be
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
  ratio_of_frozen_deposits_slashed_per_double_endorsement:
    seq:
    - id: numerator
      type: u2be
    - id: denominator
      type: u2be
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
  bool:
    0: false
    255: true
  public_key_hash_tag:
    0: ed25519
    1: secp256k1
    2: p256
seq:
- id: proof_of_work_nonce_size
  type: u1
- id: nonce_length
  type: u1
- id: max_anon_ops_per_block
  type: u1
- id: max_operation_data_length
  type: int31
- id: max_proposals_per_delegate
  type: u1
- id: max_micheline_node_count
  type: int31
- id: max_micheline_bytes_limit
  type: int31
- id: max_allowed_global_constants_depth
  type: int31
- id: cache_layout_size
  type: u1
- id: michelson_maximum_type_size
  type: u2be
- id: max_wrapped_proof_binary_size
  type: int31
- id: preserved_cycles
  type: u1
- id: blocks_per_cycle
  type: s4be
- id: blocks_per_commitment
  type: s4be
- id: nonce_revelation_threshold
  type: s4be
- id: blocks_per_stake_snapshot
  type: s4be
- id: cycles_per_voting_period
  type: s4be
- id: hard_gas_limit_per_operation
  type: z
- id: hard_gas_limit_per_block
  type: z
- id: proof_of_work_threshold
  type: s8be
- id: tokens_per_roll
  type: id_014__ptkathma__mutez
- id: vdf_difficulty
  type: s8be
- id: seed_nonce_revelation_tip
  type: id_014__ptkathma__mutez
- id: origination_size
  type: int31
- id: baking_reward_fixed_portion
  type: id_014__ptkathma__mutez
- id: baking_reward_bonus_per_slot
  type: id_014__ptkathma__mutez
- id: endorsing_reward_per_slot
  type: id_014__ptkathma__mutez
- id: cost_per_byte
  type: id_014__ptkathma__mutez
- id: hard_storage_limit_per_operation
  type: z
- id: quorum_min
  type: s4be
- id: quorum_max
  type: s4be
- id: min_proposal_quorum
  type: s4be
- id: liquidity_baking_subsidy
  type: id_014__ptkathma__mutez
- id: liquidity_baking_sunset_level
  type: s4be
- id: liquidity_baking_toggle_ema_threshold
  type: s4be
- id: max_operations_time_to_live
  type: s2be
- id: minimal_block_delay
  type: s8be
- id: delay_increment_per_round
  type: s8be
- id: consensus_committee_size
  type: int31
- id: consensus_threshold
  type: int31
- id: minimal_participation_ratio
  type: minimal_participation_ratio
- id: max_slashing_period
  type: int31
- id: frozen_deposits_percentage
  type: int31
- id: double_baking_punishment
  type: id_014__ptkathma__mutez
- id: ratio_of_frozen_deposits_slashed_per_double_endorsement
  type: ratio_of_frozen_deposits_slashed_per_double_endorsement
- id: testnet_dictator_tag
  type: u1
  enum: bool
- id: testnet_dictator
  type: public_key_hash
  if: (testnet_dictator_tag == bool::true)
  doc: A Ed25519, Secp256k1, or P256 public key hash
- id: initial_seed_tag
  type: u1
  enum: bool
- id: initial_seed
  size: 32
  if: (initial_seed_tag == bool::true)
- id: cache_script_size
  type: int31
- id: cache_stake_distribution_cycles
  type: s1
- id: cache_sampler_state_cycles
  type: s1
- id: tx_rollup_enable
  type: u1
  enum: bool
- id: tx_rollup_origination_size
  type: int31
- id: tx_rollup_hard_size_limit_per_inbox
  type: int31
- id: tx_rollup_hard_size_limit_per_message
  type: int31
- id: tx_rollup_max_withdrawals_per_batch
  type: int31
- id: tx_rollup_commitment_bond
  type: id_014__ptkathma__mutez
- id: tx_rollup_finality_period
  type: int31
- id: tx_rollup_withdraw_period
  type: int31
- id: tx_rollup_max_inboxes_count
  type: int31
- id: tx_rollup_max_messages_per_inbox
  type: int31
- id: tx_rollup_max_commitments_count
  type: int31
- id: tx_rollup_cost_per_byte_ema_factor
  type: int31
- id: tx_rollup_max_ticket_payload_size
  type: int31
- id: tx_rollup_rejection_max_proof_size
  type: int31
- id: tx_rollup_sunset_level
  type: s4be
- id: dal_parametric
  type: dal_parametric
- id: sc_rollup_enable
  type: u1
  enum: bool
- id: sc_rollup_origination_size
  type: int31
- id: sc_rollup_challenge_window_in_blocks
  type: int31
- id: sc_rollup_max_available_messages
  type: int31
- id: sc_rollup_stake_amount
  type: id_014__ptkathma__mutez
- id: sc_rollup_commitment_period_in_blocks
  type: int31
- id: sc_rollup_max_lookahead_in_blocks
  type: s4be
- id: sc_rollup_max_active_outbox_levels
  type: s4be
- id: sc_rollup_max_outbox_messages_per_level
  type: int31
