interface ZIF_SCABSRC_SOURCE_UNIT
  public .


  methods GET_INVOKING_UNIT
    returning
      value(SOURCE_UNIT) type ref to ZIF_SCABSRC_SOURCE_UNIT .
  methods GET_INVOKING_STATEMENT
    returning
      value(STATEMENT) type ref to ZIF_SCABSRC_STATEMENT .
  methods GET_STATEMENTS
    returning
      value(STATEMENTS) type ref to ZIF_SCABSRC_STATEMENTS .
  methods GET_ALL_FIELDS
    returning
      value(SOURCE_UNIT) type SLEVEL .
endinterface.
