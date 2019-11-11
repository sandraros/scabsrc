class ZCL_SCABSRC_SOURCE_UNITS definition
  public
  create public .

public section.

  interfaces ZIF_SCABSRC_SOURCE_UNITS .

  aliases GET_NEXT
    for ZIF_SCABSRC_SOURCE_UNITS~GET_NEXT .

  methods CONSTRUCTOR
    importing
      !SCABSRC type ref to ZCL_SCABSRC
      !FROM type I
      !TO type I .
protected section.
private section.

  data FROM type SYTABIX .
  data TO type SYTABIX .
  data NEXT type SYTABIX .
  data SCABSRC type ref to ZCL_SCABSRC .
ENDCLASS.



CLASS ZCL_SCABSRC_SOURCE_UNITS IMPLEMENTATION.


  method CONSTRUCTOR.

    ME->FROM = FROM.
    ME->TO   = TO.
    ME->NEXT = FROM.
    ME->SCABSRC = SCABSRC.

  endmethod.


  method ZIF_SCABSRC_SOURCE_UNITS~GET_NEXT.

    IF NEXT <= TO.
      CREATE OBJECT SOURCE_UNIT TYPE ZCL_SCABSRC_SOURCE_UNIT
        EXPORTING
          SCABSRC = SCABSRC
          INDEX   = NEXT.
      ADD 1 TO NEXT.
    ENDIF.

  endmethod.


ENDCLASS.
