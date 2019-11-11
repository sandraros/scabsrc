interface ZIF_SCABSRC_BLOCKS
  public .


  methods RESET .
  methods GET_NEXT
    returning
      value(BLOCK) type ref to ZIF_SCABSRC_BLOCK .
  methods GET_FIRST_STATEMENTS
    returning
      value(STATEMENTS) type ref to ZIF_SCABSRC_STATEMENTS .
  methods ADD_BLOCK
    importing
      !INDEX type SYTABIX .
endinterface.
