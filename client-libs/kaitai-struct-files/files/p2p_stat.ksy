meta:
  id: p2p_stat
  endian: be
doc: ! 'Encoding id: p2p_stat

  Description: Statistics about the p2p network.'
types:
  int31:
    seq:
    - id: int31
      type: s4be
      valid:
        min: -1073741824
        max: 1073741823
seq:
- id: total_sent
  type: s8be
- id: total_recv
  type: s8be
- id: current_inflow
  type: int31
- id: current_outflow
  type: int31
