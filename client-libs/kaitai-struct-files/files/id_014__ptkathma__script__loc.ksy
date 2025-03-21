meta:
  id: id_014__ptkathma__script__loc
  endian: be
doc: ! 'Encoding id: 014-PtKathma.script.loc'
types:
  int31:
    seq:
    - id: int31
      type: s4be
      valid:
        min: -1073741824
        max: 1073741823
  micheline__location:
    seq:
    - id: micheline__location
      type: int31
seq:
- id: micheline__location
  type: micheline__location
  doc: ! >-
    Canonical location in a Micheline expression: The location of a node in a Micheline
    expression tree in prefix order, with zero being the root and adding one for every
    basic node, sequence and primitive application.
