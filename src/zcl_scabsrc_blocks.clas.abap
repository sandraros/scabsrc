class ZCL_SCABSRC_BLOCKS definition
  public
  create public .

public section.

  interfaces ZIF_SCABSRC_BLOCKS .

  aliases ADD_BLOCK
    for ZIF_SCABSRC_BLOCKS~ADD_BLOCK .
  aliases GET_FIRST_STATEMENTS
    for ZIF_SCABSRC_BLOCKS~GET_FIRST_STATEMENTS .
  aliases GET_NEXT
    for ZIF_SCABSRC_BLOCKS~GET_NEXT .
  aliases RESET
    for ZIF_SCABSRC_BLOCKS~RESET .

  methods CONSTRUCTOR
    importing
      !SCABSRC type ref to ZCL_SCABSRC
      !FROM type I default 0
      !TO type I default 0 .
protected section.
private section.

  types:
    BEGIN OF TY_IS_RANGE,
              FROM  TYPE SYTABIX,
              TO    TYPE SYTABIX,
            END OF TY_IS_RANGE .

  data FROM type SYTABIX .
  data TO type SYTABIX .
  data NEXT type SYTABIX .
  data NEXT2 type SYTABIX .
  data:
    TAB_RANGE TYPE TABLE OF TY_IS_RANGE .
  data SCABSRC type ref to ZCL_SCABSRC .
  class-data TEMP type SSTRUC .
ENDCLASS.



CLASS ZCL_SCABSRC_BLOCKS IMPLEMENTATION.


  method ZIF_SCABSRC_BLOCKS~ADD_BLOCK.

    DATA RANGE TYPE TY_IS_RANGE.
    RANGE-FROM  = INDEX.
    RANGE-TO    = INDEX.
    APPEND RANGE TO TAB_RANGE.

  endmethod.


  method CONSTRUCTOR.

    ME->FROM = FROM.
    ME->TO   = TO.
    ME->SCABSRC = SCABSRC.

  endmethod.


  method ZIF_SCABSRC_BLOCKS~GET_FIRST_STATEMENTS.

    DATA LO_BLOCK TYPE REF TO ZIF_SCABSRC_BLOCK.
    DATA LO_STATEMENT TYPE REF TO ZIF_SCABSRC_STATEMENT.

    RESET( ).
    LO_BLOCK = GET_NEXT( ).
    IF LO_BLOCK IS BOUND.
      CREATE OBJECT STATEMENTS TYPE ZCL_SCABSRC_STATEMENTS
            EXPORTING
              SCABSRC = SCABSRC.
    ENDIF.
    WHILE LO_BLOCK IS BOUND.
      LO_STATEMENT = LO_BLOCK->GET_FIRST_STATEMENT( ).
      STATEMENTS->ADD_STATEMENT( LO_STATEMENT->GET_INDEX( ) ).

      LO_BLOCK = GET_NEXT( ).
    ENDWHILE.

  endmethod.


  method ZIF_SCABSRC_BLOCKS~GET_NEXT.

    FIELD-SYMBOLS <RANGE> TYPE TY_IS_RANGE.
    IF FROM >= 1.
      IF NEXT = 0.
        NEXT = FROM.
      ENDIF.
      IF NEXT <= TO.
        CREATE OBJECT BLOCK TYPE ZCL_SCABSRC_BLOCK
          EXPORTING
            SCABSRC = SCABSRC
            INDEX   = NEXT.
        ADD 1 TO NEXT.
      ENDIF.
    ELSE.
      IF NEXT = 0.
        NEXT = 1.
        NEXT2 = 0.
      ENDIF.
      DO.
        READ TABLE TAB_RANGE ASSIGNING <RANGE> INDEX NEXT.
        IF SY-SUBRC <> 0.
          EXIT.
        ENDIF.
        IF NEXT2 = 0.
          NEXT2 = <RANGE>-FROM.
        ENDIF.
        IF NEXT2 <= <RANGE>-TO.
          CREATE OBJECT BLOCK TYPE ZCL_SCABSRC_BLOCK
            EXPORTING
              SCABSRC = SCABSRC
              INDEX   = NEXT2.
          ADD 1 TO NEXT2.
          EXIT.
        ELSE.
          ADD 1 TO NEXT.
          NEXT2 = 0.
        ENDIF.
      ENDDO.
    ENDIF.

  endmethod.


  method ZIF_SCABSRC_BLOCKS~RESET.

    NEXT = 0.

  endmethod.


ENDCLASS.
