CLASS zcl_scabsrc_source_unit DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_scabsrc_source_unit .

*    ALIASES get_all_fields
*      FOR zif_scabsrc_source_unit~get_all_fields .
    ALIASES get_invoking_statement
      FOR zif_scabsrc_source_unit~get_invoking_statement .
    ALIASES get_invoking_unit
      FOR zif_scabsrc_source_unit~get_invoking_unit .
    ALIASES get_statements
      FOR zif_scabsrc_source_unit~get_statements .

    METHODS constructor
      IMPORTING
        !scabsrc TYPE REF TO zcl_scabsrc
        !index   TYPE i .
  PROTECTED SECTION.
  PRIVATE SECTION.

*    DATA index TYPE sytabix .
    DATA scabsrc TYPE REF TO zcl_scabsrc .
ENDCLASS.



CLASS zcl_scabsrc_source_unit IMPLEMENTATION.


  METHOD constructor.

    me->scabsrc = scabsrc.
    me->zif_scabsrc_source_unit~index = index.
    me->zif_scabsrc_source_unit~slevel = scabsrc->lt_slevel[ index ].

*  ENDMETHOD.
*
*
*  METHOD zif_scabsrc_source_unit~get_all_fields.
*
*    READ TABLE scabsrc->lt_slevel INDEX index INTO source_unit.

  ENDMETHOD.


  METHOD zif_scabsrc_source_unit~get_invoking_statement.

    RAISE EXCEPTION TYPE zcx_scabsrc_todo.

  ENDMETHOD.


  METHOD zif_scabsrc_source_unit~get_invoking_unit.

    RAISE EXCEPTION TYPE zcx_scabsrc_todo.

  ENDMETHOD.


  METHOD zif_scabsrc_source_unit~get_statements.

    statements = zcl_scabsrc_statements=>create_for_source_unit(
        scabsrc = scabsrc
        source_unit = me ).

  ENDMETHOD.


ENDCLASS.
