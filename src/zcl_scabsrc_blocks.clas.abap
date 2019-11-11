"! <p class="shorttext synchronized" lang="en"></p>
"!
CLASS zcl_scabsrc_blocks DEFINITION
  PUBLIC
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_scabsrc_blocks .

    ALIASES get_first_statement_each_block
      FOR zif_scabsrc_blocks~get_first_statement_each_block .
    ALIASES get_next
      FOR zif_scabsrc_blocks~get_next .
    ALIASES reset
      FOR zif_scabsrc_blocks~reset .
    ALIASES count
      FOR zif_scabsrc_blocks~count .

    CLASS-METHODS create_for_children_blocks
      IMPORTING
        block         TYPE REF TO zif_scabsrc_block
      RETURNING
        VALUE(blocks) TYPE REF TO zif_scabsrc_blocks.

    CLASS-METHODS create
      IMPORTING
        scabsrc        TYPE REF TO zcl_scabsrc
        type           TYPE zif_scabsrc_block=>ty_type OPTIONAL
        stmnt_type     TYPE zif_scabsrc_block=>ty_stmnt_type OPTIONAL
        rng_type       TYPE zif_scabsrc_block=>ty_rng_type OPTIONAL
        rng_stmnt_type TYPE zif_scabsrc_block=>ty_rng_stmnt_type OPTIONAL
      RETURNING
        VALUE(blocks)  TYPE REF TO zif_scabsrc_blocks.

    TYPES ty_rng_block_index TYPE RANGE OF sytabix.

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
    DATA scabsrc TYPE REF TO zcl_scabsrc .
    CLASS-DATA temp TYPE sstruc .
ENDCLASS.



CLASS zcl_scabsrc_blocks IMPLEMENTATION.


  METHOD constructor.

    me->tab_range = tab_range.
    me->scabsrc = scabsrc.
    me->count = REDUCE #( INIT i = 0 FOR line IN tab_range NEXT i = i + line-to - line-from + 1 ).

  ENDMETHOD.


  METHOD zif_scabsrc_blocks~get_first_statement_each_block.

    DATA lo_block TYPE REF TO zif_scabsrc_block.
    DATA lo_statement TYPE REF TO zif_scabsrc_statement.

    reset( ).
    lo_block = get_next( ).
    WHILE lo_block IS BOUND.
      lo_statement = lo_block->get_first_statement( ).
      APPEND lo_statement TO statements.
      lo_block = get_next( ).
    ENDWHILE.

  ENDMETHOD.


  METHOD zif_scabsrc_blocks~get_next.

    FIELD-SYMBOLS <range> TYPE ty_is_range.

    IF next = 0.
      next = 1.
      next2 = 0.
    ENDIF.
    DO.
      READ TABLE tab_range ASSIGNING <range> INDEX next.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
      IF next2 = 0.
        next2 = <range>-from.
      ENDIF.
      IF next2 <= <range>-to.
        CREATE OBJECT block TYPE zcl_scabsrc_block
          EXPORTING
            scabsrc = scabsrc
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

    FIELD-SYMBOLS <ls_sstruc> TYPE sstruc.
    READ TABLE block->scabsrc->lt_sstruc INDEX block->get_index( ) ASSIGNING <ls_sstruc>.
    ASSERT sy-subrc = 0.

    CREATE OBJECT blocks TYPE zcl_scabsrc_blocks
      EXPORTING
        scabsrc   = block->scabsrc
        tab_range = VALUE #( ( from = <ls_sstruc>-struc_from to = <ls_sstruc>-struc_to ) ).

  ENDMETHOD.

ENDCLASS.
