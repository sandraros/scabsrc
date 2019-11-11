interface ZIF_SCABSRC_TOKEN
  public .


  methods GET_STATEMENT
    returning
      value(STATEMENT) type ref to ZIF_SCABSRC_STATEMENT .
  methods GET_ALL_FIELDS
    returning
      value(TOKEN) type STOKESX .
endinterface.
