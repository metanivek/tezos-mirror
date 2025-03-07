meta:
  id: id_006__pscartha__constants__fixed
  endian: be
doc: ! 'Encoding id: 006-PsCARTHA.constants.fixed'
types:
  int31:
    seq:
    - id: int31
      type: s4be
      valid:
        min: -1073741824
        max: 1073741823
seq:
- id: proof_of_work_nonce_size
  type: u1
- id: nonce_length
  type: u1
- id: max_revelations_per_block
  type: u1
- id: max_operation_data_length
  type: int31
- id: max_proposals_per_delegate
  type: u1
