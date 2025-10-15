meta:
  id: tallinn__block_header__unsigned
  endian: be
  imports:
  - block_header__shell
doc: ! 'Encoding id: tallinn.block_header.unsigned'
types:
  tallinn__block_header__alpha__unsigned_contents:
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
      type: tallinn__per_block_votes
  tallinn__per_block_votes:
    seq:
    - id: tallinn__per_block_votes_tag
      type: u1
      enum: tallinn__per_block_votes_tag
enums:
  bool:
    0: false
    255: true
  tallinn__per_block_votes_tag:
    0: per_block_vote_on
    1: per_block_vote_off
    2: per_block_vote_pass
seq:
- id: tallinn__block_header__unsigned
  type: block_header__shell
- id: tallinn__block_header__alpha__unsigned_contents
  type: tallinn__block_header__alpha__unsigned_contents
