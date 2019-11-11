"! <p class="shorttext synchronized" lang="en"></p>
"!
CLASS zcl_scabsrc_source_units DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_scabsrc_source_units .

    ALIASES get_next
      FOR zif_scabsrc_source_units~get_next .

    METHODS constructor
      IMPORTING
        !scabsrc TYPE REF TO zcl_scabsrc
        !from    TYPE i DEFAULT 1
        !to      TYPE i OPTIONAL.
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA from TYPE sytabix .
    DATA to TYPE sytabix .
    DATA next TYPE sytabix .
    DATA scabsrc TYPE REF TO zcl_scabsrc .
ENDCLASS.



CLASS zcl_scabsrc_source_units IMPLEMENTATION.


  METHOD constructor.

    me->from = from.
    IF to IS NOT INITIAL.
      me->to   = to.
    ELSE.
      me->to   = lines( scabsrc->lt_slevel ).
    ENDIF.
    me->next = from.
    me->scabsrc = scabsrc.

  ENDMETHOD.


  METHOD zif_scabsrc_source_units~get_next.

    IF next <= to.
      CREATE OBJECT source_unit TYPE zcl_scabsrc_source_unit
        EXPORTING
          scabsrc = scabsrc
          index   = next.
      ADD 1 TO next.
    ENDIF.

  ENDMETHOD.


ENDCLASS.
