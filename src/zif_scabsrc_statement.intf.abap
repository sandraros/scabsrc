interface ZIF_SCABSRC_STATEMENT
  public .


  methods GET_TOKENS
    returning
      value(TOKENS) type ref to ZIF_SCABSRC_TOKENS .
  methods GET_BLOCK
    returning
      value(BLOCK) type ref to ZIF_SCABSRC_BLOCK .
  methods GET_SOURCE_UNIT
    returning
      value(SOURCE_UNIT) type ref to ZIF_SCABSRC_SOURCE_UNIT .
  methods GET_BLOCKS
    importing
      !TYPE type SSTRUC-TYPE optional
      !STMNT_TYPE type SSTRUC-STMNT_TYPE optional
    returning
      value(BLOCKS) type ref to ZIF_SCABSRC_BLOCKS .
  methods GET_ALL_FIELDS
    returning
      value(STATEMENT) type SSTMNT .
  methods GET_INDEX
    returning
      value(INDEX) type SYTABIX .
endinterface.
