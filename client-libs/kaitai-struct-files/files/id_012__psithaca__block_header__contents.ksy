meta:
  id: id_012__psithaca__block_header__contents
  endian: be
doc: ! 'Encoding id: 012-Psithaca.block_header.contents'
types:
  id_012__psithaca__block_header__alpha__unsigned_contents:
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
    - id: liquidity_baking_escape_vote
      type: u1
      enum: bool
enums:
  bool:
    0: false
    255: true
seq:
- id: id_012__psithaca__block_header__alpha__unsigned_contents
  type: id_012__psithaca__block_header__alpha__unsigned_contents
