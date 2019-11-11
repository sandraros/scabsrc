interface ZIF_SCABSRC_TOKENS
  public .


  methods GET_NEXT
    returning
      value(TOKEN) type ref to ZIF_SCABSRC_TOKEN .
endinterface.
