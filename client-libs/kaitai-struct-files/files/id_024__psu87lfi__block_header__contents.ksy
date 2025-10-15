meta:
  id: id_024__psu87lfi__block_header__contents
  endian: be
doc: ! 'Encoding id: 024-PsU87LFi.block_header.contents'
types:
  id_024__psu87lfi__block_header__alpha__unsigned_contents:
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
    - id: per_block_votes
      type: id_024__psu87lfi__per_block_votes
  id_024__psu87lfi__per_block_votes:
    seq:
    - id: id_024__psu87lfi__per_block_votes_tag
      type: u1
      enum: id_024__psu87lfi__per_block_votes_tag
enums:
  bool:
    0: false
    255: true
  id_024__psu87lfi__per_block_votes_tag:
    0: per_block_vote_on
    1: per_block_vote_off
    2: per_block_vote_pass
seq:
- id: id_024__psu87lfi__block_header__alpha__unsigned_contents
  type: id_024__psu87lfi__block_header__alpha__unsigned_contents
