interface ZIF_SCABSRC_BLOCK
  public .


  methods GET_STATEMENTS
    returning
      value(STATEMENTS) type ref to ZIF_SCABSRC_STATEMENTS .
  methods GET_CHILD_BLOCKS
    returning
      value(BLOCKS) type ref to ZIF_SCABSRC_BLOCKS .
  methods GET_PARENT_BLOCK
    returning
      value(BLOCK) type ref to ZIF_SCABSRC_BLOCK .
  methods GET_ALL_FIELDS
    returning
      value(BLOCK) type SSTRUC .
  methods GET_INDEX
    returning
      value(INDEX) type SYTABIX .
  methods GET_TYPE
    returning
      value(TYPE) type SSTRUC-TYPE .
  methods GET_STMNT_TYPE
    returning
      value(STMNT_TYPE) type SSTRUC-STMNT_TYPE .
  methods GET_FIRST_STATEMENT
    returning
      value(STATEMENT) type ref to ZIF_SCABSRC_STATEMENT .
endinterface.
