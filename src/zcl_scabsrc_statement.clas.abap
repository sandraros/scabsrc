"! <p class="shorttext synchronized" lang="en"></p>
"!
CLASS zcl_scabsrc_statement DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_scabsrc_statement .

    ALIASES get_all_fields
      FOR zif_scabsrc_statement~get_all_fields .
    ALIASES get_block
      FOR zif_scabsrc_statement~get_block .
    ALIASES get_blocks
      FOR zif_scabsrc_statement~get_blocks .
    ALIASES get_index
      FOR zif_scabsrc_statement~get_index .
    ALIASES get_source_unit
      FOR zif_scabsrc_statement~get_source_unit .
    ALIASES get_tokens
      FOR zif_scabsrc_statement~get_tokens .

    METHODS constructor
      IMPORTING
        !scabsrc TYPE REF TO zcl_scabsrc
        !index   TYPE i .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA index TYPE sytabix .
    DATA scabsrc TYPE REF TO zcl_scabsrc .
ENDCLASS.



CLASS zcl_scabsrc_statement IMPLEMENTATION.


  METHOD constructor.

    me->index = index.
    me->scabsrc = scabsrc.

  ENDMETHOD.


  METHOD zif_scabsrc_statement~get_all_fields.

    READ TABLE scabsrc->lt_sstmnt INDEX index INTO statement.

  ENDMETHOD.


  METHOD zif_scabsrc_statement~get_block.

    FIELD-SYMBOLS <ls_sstmnt> TYPE sstmnt.
    READ TABLE scabsrc->lt_sstmnt INDEX index ASSIGNING <ls_sstmnt>.
    IF sy-subrc = 0.
      CREATE OBJECT block TYPE zcl_scabsrc_block
        EXPORTING
          scabsrc = scabsrc
          index   = <ls_sstmnt>-struc.
    ENDIF.

  ENDMETHOD.


  METHOD zif_scabsrc_statement~get_blocks.

*    DATA lo_block TYPE REF TO zif_scabsrc_block.
*
*    lo_block = get_block( ).
*    IF lo_block IS BOUND.
*      CREATE OBJECT blocks TYPE zcl_scabsrc_blocks
*        EXPORTING
*          scabsrc = scabsrc.
*    ENDIF.
*    WHILE lo_block IS BOUND.
*      blocks->add_block( lo_block->get_index( ) ).
*      IF ( type IS SUPPLIED AND type = lo_block->get_type( ) )
*            OR ( stmnt_type IS SUPPLIED AND stmnt_type = lo_block->get_stmnt_type( ) ).
*        EXIT.
*      ENDIF.
*      lo_block = lo_block->get_parent_block( ).
*    ENDWHILE.

  ENDMETHOD.


  METHOD zif_scabsrc_statement~get_index.

    index = me->index.

  ENDMETHOD.


  METHOD zif_scabsrc_statement~get_source_unit.

    FIELD-SYMBOLS <ls_sstmnt> TYPE sstmnt.
    READ TABLE scabsrc->lt_sstmnt INDEX index ASSIGNING <ls_sstmnt>.
    IF sy-subrc = 0.
      CREATE OBJECT source_unit TYPE zcl_scabsrc_source_unit
        EXPORTING
          scabsrc = scabsrc
          index   = <ls_sstmnt>-level.
    ENDIF.

  ENDMETHOD.


  METHOD zif_scabsrc_statement~get_tokens.

    ASSIGN scabsrc->lt_sstmnt[ index ] TO FIELD-SYMBOL(<ls_sstmnt>).
    ASSERT sy-subrc = 0.

    CREATE OBJECT tokens TYPE zcl_scabsrc_tokens
      EXPORTING
        scabsrc = scabsrc
        from    = <ls_sstmnt>-from
        to      = <ls_sstmnt>-to.

  ENDMETHOD.


  METHOD zif_scabsrc_statement~get_token.

    ASSIGN scabsrc->lt_sstmnt[ index ] TO FIELD-SYMBOL(<ls_sstmnt>).
    ASSERT sy-subrc = 0.

    DATA(index_token) = <ls_sstmnt>-from + position - 1.
    IF index_token BETWEEN <ls_sstmnt>-from AND <ls_sstmnt>-to.
      CREATE OBJECT token TYPE zcl_scabsrc_token
        EXPORTING
          scabsrc = scabsrc
          index   = index_token.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
