"! <p class="shorttext synchronized" lang="en"></p>
"!
class ZCL_SCABSRC_BLOCKS definition
  public
  create private .

public section.

  interfaces ZIF_SCABSRC_BLOCKS .

  types:
    ty_rng_block_index TYPE RANGE OF sytabix .

*    ALIASES get_first_statement_each_block
*      FOR zif_scabsrc_blocks~get_first_statement_each_block .
*    ALIASES get_next
*      FOR zif_scabsrc_blocks~get_next .
*    ALIASES reset
*      FOR zif_scabsrc_blocks~reset .
  class-methods CREATE_FOR_CHILDREN_BLOCKS
    importing
      !BLOCK type ref to ZIF_SCABSRC_BLOCK
    returning
      value(BLOCKS) type ref to ZIF_SCABSRC_BLOCKS .
  class-methods CREATE
    importing
      !SCABSRC type ref to ZCL_SCABSRC
      !TYPE type ZIF_SCABSRC_BLOCK=>TY_TYPE optional
      !STMNT_TYPE type ZIF_SCABSRC_BLOCK=>TY_STMNT_TYPE optional
      !RNG_TYPE type ZIF_SCABSRC_BLOCK=>TY_RNG_TYPE optional
      !RNG_STMNT_TYPE type ZIF_SCABSRC_BLOCK=>TY_RNG_STMNT_TYPE optional
    returning
      value(BLOCKS) type ref to ZIF_SCABSRC_BLOCKS .
  class-methods CREATE_FOR_STATEMENT
    importing
      !STATEMENT type ref to ZIF_SCABSRC_STATEMENT
      !TYPE type ZIF_SCABSRC_BLOCK=>TY_TYPE optional
      !STMNT_TYPE type ZIF_SCABSRC_BLOCK=>TY_STMNT_TYPE optional
      !RNG_TYPE type ZIF_SCABSRC_BLOCK=>TY_RNG_TYPE optional
      !RNG_STMNT_TYPE type ZIF_SCABSRC_BLOCK=>TY_RNG_STMNT_TYPE optional
    returning
      value(BLOCKS) type ref to ZIF_SCABSRC_BLOCKS .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_is_range,
        from TYPE sytabix,
        to   TYPE sytabix,
      END OF ty_is_range,
      ty_it_range TYPE STANDARD TABLE OF ty_is_range WITH EMPTY KEY.

    METHODS constructor
      IMPORTING
        !scabsrc   TYPE REF TO zcl_scabsrc
        !tab_range TYPE ty_it_range.

    DATA next TYPE sytabix .
    DATA next2 TYPE sytabix .
    DATA tab_range TYPE ty_it_range .
ENDCLASS.



CLASS ZCL_SCABSRC_BLOCKS IMPLEMENTATION.


  METHOD constructor.

    me->tab_range = tab_range.
    me->zif_scabsrc_blocks~scabsrc = scabsrc.
    me->zif_scabsrc_blocks~count = REDUCE #( INIT i = 0 FOR line IN tab_range NEXT i = i + line-to - line-from + 1 ).

  ENDMETHOD.


  METHOD create.

    DATA: tab_range TYPE ty_it_range.

    IF type IS INITIAL AND stmnt_type IS INITIAL
    AND rng_type IS INITIAL AND rng_stmnt_type IS INITIAL.

      tab_range = VALUE #( ( from = 1 to = lines( scabsrc->lt_sstruc ) ) ).

    ELSE.

      DATA(sng_rng_type) = COND zif_scabsrc_block=>ty_rng_type(
        WHEN type IS NOT INITIAL THEN VALUE #( ( sign = 'I' option = 'EQ' low = type ) ) ).
      DATA(sng_rng_stmnt_type) = COND zif_scabsrc_block=>ty_rng_stmnt_type(
        WHEN stmnt_type IS NOT INITIAL THEN VALUE #( ( sign = 'I' option = 'EQ' low = stmnt_type ) ) ).

      LOOP AT scabsrc->lt_sstruc ASSIGNING FIELD-SYMBOL(<ls_sstruc>)
            WHERE type       IN rng_type
              AND type       IN sng_rng_type
              AND stmnt_type IN rng_stmnt_type
              AND stmnt_type IN sng_rng_stmnt_type.
        tab_range = VALUE #( BASE tab_range ( from = sy-tabix to = sy-tabix ) ).
      ENDLOOP.

    ENDIF.

    blocks = NEW zcl_scabsrc_blocks( scabsrc = scabsrc tab_range = tab_range ).

  ENDMETHOD.


  METHOD create_for_children_blocks.

    CREATE OBJECT blocks TYPE zcl_scabsrc_blocks
      EXPORTING
        scabsrc   = block->scabsrc
        tab_range = VALUE #( ( from = block->sstruc-struc_from to = block->sstruc-struc_to ) ).

  ENDMETHOD.


  METHOD create_for_statement.

    DATA(block) = statement->get_block( ).

    DATA(temp_tab_range) = VALUE ty_it_range( ).
    WHILE block IS BOUND.
      IF ( type IS NOT INITIAL AND type = block->sstruc-type )
        OR ( type IS INITIAL AND block->sstruc-type IN rng_type )
        OR ( stmnt_type IS NOT INITIAL AND stmnt_type = block->sstruc-stmnt_type )
        OR ( stmnt_type IS INITIAL AND block->sstruc-stmnt_type IN rng_stmnt_type ).
        temp_tab_range = VALUE #( BASE temp_tab_range ( from = block->index to = block->index ) ).
      ENDIF.
      block = block->get_parent_block( ).
    ENDWHILE.

  ENDMETHOD.


  METHOD zif_scabsrc_blocks~get_first_statement_each_block.

    zif_scabsrc_blocks~reset( ).
    DATA(block) = zif_scabsrc_blocks~get_next( ).
    WHILE block IS BOUND.
      APPEND block->get_first_statement( ) TO statements.
      block = zif_scabsrc_blocks~get_next( ).
    ENDWHILE.
    zif_scabsrc_blocks~reset( ).

  ENDMETHOD.


  METHOD zif_scabsrc_blocks~get_next.

    IF next = 0.
      next = 1.
      next2 = 0.
    ENDIF.
    DO.
      ASSIGN tab_range[ next ] TO FIELD-SYMBOL(<range>).
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
      IF next2 = 0.
        next2 = <range>-from.
      ENDIF.
      IF next2 <= <range>-to.
        CREATE OBJECT block TYPE zcl_scabsrc_block
          EXPORTING
            scabsrc = zif_scabsrc_blocks~scabsrc
            index   = next2.
        ADD 1 TO next2.
        EXIT.
      ELSE.
        ADD 1 TO next.
        next2 = 0.
      ENDIF.
    ENDDO.

  ENDMETHOD.


  METHOD zif_scabsrc_blocks~reset.

    next = 0.

  ENDMETHOD.
ENDCLASS.
