"! <p class="shorttext synchronized" lang="en"></p>
"!
CLASS zcl_scabsrc_block DEFINITION
  PUBLIC
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_scabsrc zcl_scabsrc_blocks zcl_scabsrc_statement zcl_scabsrc_statements.

  PUBLIC SECTION.

    INTERFACES zif_scabsrc_block .

*    ALIASES get_all_fields
*      FOR zif_scabsrc_block~get_all_fields .
    ALIASES get_child_blocks
      FOR zif_scabsrc_block~get_child_blocks .
    ALIASES get_first_statement
      FOR zif_scabsrc_block~get_first_statement .
*    ALIASES get_index
*      FOR zif_scabsrc_block~get_index .
    ALIASES get_parent_block
      FOR zif_scabsrc_block~get_parent_block .
    ALIASES get_statements
      FOR zif_scabsrc_block~get_statements .
    ALIASES get_stmnt_type
      FOR zif_scabsrc_block~get_stmnt_type .
    ALIASES get_type
      FOR zif_scabsrc_block~get_type .
*    ALIASES scabsrc
*      FOR zif_scabsrc_block~scabsrc .

    METHODS constructor
      IMPORTING
        !scabsrc TYPE REF TO zcl_scabsrc
        !index   TYPE i .
  PROTECTED SECTION.
  PRIVATE SECTION.

*    DATA index TYPE sytabix .
    DATA scabsrc TYPE REF TO zcl_scabsrc .
    DATA temp TYPE sstruc .

ENDCLASS.



CLASS zcl_scabsrc_block IMPLEMENTATION.


  METHOD constructor.

    me->scabsrc = scabsrc.
    me->zif_scabsrc_block~index = index.
    me->zif_scabsrc_block~sstruc = scabsrc->lt_sstruc[ index ].

*  ENDMETHOD.
*
*
*  METHOD zif_scabsrc_block~get_all_fields.
*
*    READ TABLE scabsrc->lt_sstruc INDEX index INTO block.

  ENDMETHOD.


  METHOD zif_scabsrc_block~get_child_blocks.

    blocks = zcl_scabsrc_blocks=>create_for_children_blocks( me ).

  ENDMETHOD.


  METHOD zif_scabsrc_block~get_first_statement.

    CREATE OBJECT statement TYPE zcl_scabsrc_statement
      EXPORTING
        scabsrc = scabsrc
        index   = zif_scabsrc_block~sstruc-stmnt_from.

*  ENDMETHOD.
*
*
*  METHOD zif_scabsrc_block~get_index.
*
*    index = me->index.

  ENDMETHOD.


  METHOD zif_scabsrc_block~get_parent_block.

*    FIELD-SYMBOLS <ls_sstruc> TYPE sstruc.
*    READ TABLE scabsrc->lt_sstruc INDEX index ASSIGNING <ls_sstruc>.
*    IF sy-subrc = 0.
    CREATE OBJECT block TYPE zcl_scabsrc_block
      EXPORTING
        scabsrc = scabsrc
        index   = zif_scabsrc_block~sstruc-back.
*    ENDIF.

  ENDMETHOD.


  METHOD zif_scabsrc_block~get_statements.

*    FIELD-SYMBOLS <ls_sstruc> TYPE sstruc.
*    READ TABLE scabsrc->lt_sstruc INDEX index ASSIGNING <ls_sstruc>.
*    IF sy-subrc = 0.
    statements = zcl_scabsrc_statements=>create_for_block(
        scabsrc = scabsrc
        block   = me ).
*    ENDIF.

  ENDMETHOD.


  METHOD zif_scabsrc_block~get_stmnt_type.

*    READ TABLE scabsrc->lt_sstruc INDEX index INTO temp TRANSPORTING stmnt_type.
*    stmnt_type = temp-stmnt_type.
    stmnt_type = zif_scabsrc_block~sstruc-stmnt_type.

  ENDMETHOD.


  METHOD zif_scabsrc_block~get_type.

*    READ TABLE scabsrc->lt_sstruc INDEX index INTO temp TRANSPORTING type.
*    type = temp-type.
    type = zif_scabsrc_block~sstruc-type.

  ENDMETHOD.


ENDCLASS.
