meta:
  id: id_011__pthangz2__block_header__unsigned
  endian: be
  imports:
  - block_header__shell
doc: ! 'Encoding id: 011-PtHangz2.block_header.unsigned'
types:
  id_011__pthangz2__block_header__alpha__unsigned_contents:
    seq:
    - id: priority
      type: u2be
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
- id: id_011__pthangz2__block_header__unsigned
  type: block_header__shell
- id: id_011__pthangz2__block_header__alpha__unsigned_contents
  type: id_011__pthangz2__block_header__alpha__unsigned_contents
