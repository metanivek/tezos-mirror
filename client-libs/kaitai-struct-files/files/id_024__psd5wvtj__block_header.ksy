meta:
  id: id_024__psd5wvtj__block_header
  endian: be
  imports:
  - block_header__shell
doc: ! 'Encoding id: 024-PsD5wVTJ.block_header'
types:
  id_024__psd5wvtj__block_header__alpha__full_header:
    seq:
    - id: id_024__psd5wvtj__block_header__alpha__full_header
      type: block_header__shell
    - id: id_024__psd5wvtj__block_header__alpha__signed_contents
      type: id_024__psd5wvtj__block_header__alpha__signed_contents
  id_024__psd5wvtj__block_header__alpha__signed_contents:
    seq:
    - id: id_024__psd5wvtj__block_header__alpha__unsigned_contents
      type: id_024__psd5wvtj__block_header__alpha__unsigned_contents
    - id: signature
      size-eos: true
  id_024__psd5wvtj__block_header__alpha__unsigned_contents:
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
      type: id_024__psd5wvtj__per_block_votes
  id_024__psd5wvtj__per_block_votes:
    seq:
    - id: id_024__psd5wvtj__per_block_votes_tag
      type: u1
      enum: id_024__psd5wvtj__per_block_votes_tag
enums:
  bool:
    0: false
    255: true
  id_024__psd5wvtj__per_block_votes_tag:
    0: per_block_vote_on
    1: per_block_vote_off
    2: per_block_vote_pass
seq:
- id: id_024__psd5wvtj__block_header__alpha__full_header
  type: id_024__psd5wvtj__block_header__alpha__full_header
