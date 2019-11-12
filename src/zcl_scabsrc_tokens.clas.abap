CLASS zcl_scabsrc_tokens DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_scabsrc_tokens .

*    ALIASES get_next
*      FOR zif_scabsrc_tokens~get_next .
*    ALIASES count
*      FOR zif_scabsrc_tokens~count .

    METHODS constructor
      IMPORTING
        !scabsrc TYPE REF TO zcl_scabsrc
        !from    TYPE i
        !to      TYPE i .

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA from TYPE sytabix .
    DATA to TYPE sytabix .
    DATA next TYPE sytabix .
ENDCLASS.



CLASS zcl_scabsrc_tokens IMPLEMENTATION.


  METHOD constructor.

    me->from = from.
    me->to   = to.
    me->next = from.
    me->zif_scabsrc_tokens~scabsrc = scabsrc.
    me->zif_scabsrc_tokens~count = to - from + 1.

  ENDMETHOD.


  METHOD zif_scabsrc_tokens~reset.

    next = from + position - 1.

  ENDMETHOD.


  METHOD zif_scabsrc_tokens~get_next.

    IF next <= to.
      CREATE OBJECT token TYPE zcl_scabsrc_token
        EXPORTING
          scabsrc = zif_scabsrc_tokens~scabsrc
          index   = next.
      ADD 1 TO next.
    ENDIF.

  ENDMETHOD.


  METHOD zif_scabsrc_tokens~get_token.

    DATA(index) = from + position - 1.
    IF index BETWEEN from AND to.
      CREATE OBJECT token TYPE zcl_scabsrc_token
        EXPORTING
          scabsrc = zif_scabsrc_tokens~scabsrc
          index   = index.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
