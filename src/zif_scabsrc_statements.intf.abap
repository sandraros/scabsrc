interface ZIF_SCABSRC_STATEMENTS
  public .


  methods GET_NEXT
    returning
      value(STATEMENT) type ref to ZIF_SCABSRC_STATEMENT .
  methods FULL_TEXT_SEARCH
    importing
      !REGEX type CSEQUENCE
    returning
      value(STATEMENTS) type ref to ZIF_SCABSRC_STATEMENTS .
  methods ADD_STATEMENT
    importing
      !INDEX type SYTABIX .
  methods ADD_STATEMENTS
    importing
      !FROM type SYTABIX
      !TO type SYTABIX .
endinterface.
