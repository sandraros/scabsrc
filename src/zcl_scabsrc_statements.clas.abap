"! <p class="shorttext synchronized" lang="en"></p>
"!
CLASS zcl_scabsrc_statements DEFINITION
  PUBLIC
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_scabsrc_statements .

    ALIASES full_text_search
      FOR zif_scabsrc_statements~full_text_search .
    ALIASES get_next
      FOR zif_scabsrc_statements~get_next .
    ALIASES count
      FOR zif_scabsrc_statements~count .

    CLASS-METHODS create
      IMPORTING
        scabsrc           TYPE REF TO zcl_scabsrc
        type              TYPE zif_scabsrc_statement=>ty_type OPTIONAL
        rng_type          TYPE zif_scabsrc_statement=>ty_rng_type OPTIONAL
      RETURNING
        VALUE(statements) TYPE REF TO zif_scabsrc_statements.

    CLASS-METHODS create_for_block
      IMPORTING
        scabsrc           TYPE REF TO zcl_scabsrc
        block             TYPE REF TO zcl_scabsrc_block
      RETURNING
        VALUE(statements) TYPE REF TO zif_scabsrc_statements.

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
ENDCLASS.



CLASS zcl_scabsrc_statements IMPLEMENTATION.


  METHOD create.

    DATA: tab_range TYPE ty_it_range.

    IF type IS INITIAL AND rng_type IS INITIAL.

      tab_range = VALUE #( ( from = 1 to = lines( scabsrc->lt_sstmnt ) ) ).

    ELSE.

      DATA(sng_rng_type) = COND zif_scabsrc_statement=>ty_rng_type(
        WHEN type IS NOT INITIAL THEN VALUE #( ( sign = 'I' option = 'EQ' low = type ) ) ).

      LOOP AT scabsrc->lt_sstmnt TRANSPORTING NO FIELDS
            WHERE type       IN rng_type
              AND type       IN sng_rng_type.
        tab_range = VALUE #( BASE tab_range ( from = sy-tabix to = sy-tabix ) ).
      ENDLOOP.

    ENDIF.

    statements = NEW zcl_scabsrc_statements( scabsrc = scabsrc tab_range = tab_range ).

  ENDMETHOD.


  METHOD constructor.

    me->tab_range = tab_range.
    me->scabsrc = scabsrc.
    me->count = REDUCE #( INIT i = 0 FOR line IN tab_range NEXT i = i + line-to - line-from + 1 ).
    me->next = 0.

  ENDMETHOD.


  METHOD zif_scabsrc_statements~full_text_search.

    DATA lo_statement TYPE REF TO zif_scabsrc_statement.
    DATA index TYPE sytabix.
    DATA full_statement TYPE string.
    DATA tab_stmnt_index TYPE TABLE OF sytabix.
    DATA tab_full_text TYPE TABLE OF string.
    DATA ls_sstmnt TYPE sstmnt.
    FIELD-SYMBOLS <ls_stokesx> TYPE stokesx.
    DATA lt_match TYPE match_result_tab.
    FIELD-SYMBOLS <ls_match> TYPE match_result.

    lo_statement = get_next( ).
    WHILE lo_statement IS BOUND.
      index = lo_statement->get_index( ).
      ls_sstmnt = lo_statement->get_all_fields( ).
      READ TABLE tab_stmnt_index WITH KEY table_line = index TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        APPEND index TO tab_stmnt_index.
        CLEAR full_statement.
        LOOP AT scabsrc->lt_stokesx ASSIGNING <ls_stokesx>
              FROM ls_sstmnt-from
              TO   ls_sstmnt-to.
          IF full_statement IS INITIAL.
            full_statement = <ls_stokesx>-str.
          ELSE.
            CONCATENATE full_statement <ls_stokesx>-str INTO full_statement
                  SEPARATED BY space.
          ENDIF.
        ENDLOOP.
        APPEND full_statement TO tab_full_text.
      ENDIF.
      lo_statement = get_next( ).
    ENDWHILE.

    FIND ALL OCCURRENCES OF REGEX regex IN TABLE tab_full_text RESULTS lt_match.
    LOOP AT lt_match ASSIGNING <ls_match>.
      ...
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_scabsrc_statements~get_next.

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
        CREATE OBJECT statement TYPE zcl_scabsrc_statement
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

  METHOD create_for_block.

    ASSIGN scabsrc->lt_sstruc[ block->index ] TO FIELD-SYMBOL(<sstruc>).
    ASSERT sy-subrc = 0.

    statements = NEW zcl_scabsrc_statements(
        scabsrc   = scabsrc
        tab_range = VALUE #( ( from = <sstruc>-stmnt_from to = <sstruc>-stmnt_to ) ) ).

  ENDMETHOD.

ENDCLASS.
