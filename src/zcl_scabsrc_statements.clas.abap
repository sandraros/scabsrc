"! <p class="shorttext synchronized" lang="en"></p>
"!
CLASS zcl_scabsrc_statements DEFINITION
  PUBLIC
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_scabsrc_statements .

    ALIASES count
      FOR zif_scabsrc_statements~count .
    ALIASES full_text_search
      FOR zif_scabsrc_statements~full_text_search .
    ALIASES reset
      FOR zif_scabsrc_statements~reset .
    ALIASES get_next
      FOR zif_scabsrc_statements~get_next .

    CLASS-METHODS create
      IMPORTING
        !scabsrc          TYPE REF TO zcl_scabsrc
        !type             TYPE zif_scabsrc_statement=>ty_type OPTIONAL
        !rng_type         TYPE zif_scabsrc_statement=>ty_rng_type OPTIONAL
      RETURNING
        VALUE(statements) TYPE REF TO zif_scabsrc_statements .
    CLASS-METHODS create_for_block
      IMPORTING
        !scabsrc          TYPE REF TO zcl_scabsrc
        !block            TYPE REF TO zif_scabsrc_block
      RETURNING
        VALUE(statements) TYPE REF TO zif_scabsrc_statements .
    CLASS-METHODS create_for_source_unit
      IMPORTING
        !scabsrc          TYPE REF TO zcl_scabsrc
        !source_unit      TYPE REF TO zif_scabsrc_source_unit
      RETURNING
        VALUE(statements) TYPE REF TO zif_scabsrc_statements .
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


  METHOD constructor.

    me->tab_range = tab_range.
    me->scabsrc = scabsrc.
    me->count = REDUCE #( INIT i = 0 FOR line IN tab_range NEXT i = i + line-to - line-from + 1 ).
    me->next = 0.

  ENDMETHOD.


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


  METHOD create_for_block.

    statements = NEW zcl_scabsrc_statements(
        scabsrc   = scabsrc
        tab_range = VALUE #( ( from = block->sstruc-stmnt_from to = block->sstruc-stmnt_to ) ) ).

  ENDMETHOD.


  METHOD create_for_source_unit.

    statements = NEW zcl_scabsrc_statements(
          scabsrc   = scabsrc
          tab_range = VALUE #( ( from = source_unit->slevel-from to = source_unit->slevel-to ) ) ).

  ENDMETHOD.


  METHOD zif_scabsrc_statements~full_text_search.

    DATA(temp_tab_range) = VALUE ty_it_range( ).

    reset( ).
    DO zif_scabsrc_statements~count TIMES.
      DATA(statement) = get_next( ).

      DATA(tokens_string) = REDUCE string(
        INIT str = ``
        FOR <stokesx> IN scabsrc->lt_stokesx
            FROM statement->sstmnt-from
            TO   nmin( val1 = statement->sstmnt-from + limit_tokens - 1 val2 = statement->sstmnt-to )
        NEXT str = COND #(
            WHEN str IS INITIAL THEN <stokesx>-str
            ELSE |{ str }\n{ <stokesx>-str }| ) ).

      IF matches( val = tokens_string regex = regex ).
        temp_tab_range = VALUE #( BASE temp_tab_range ( from = statement->index to = statement->index  ) ).
      ENDIF.

    ENDDO.

    statements = NEW zcl_scabsrc_statements( scabsrc = me->scabsrc tab_range = temp_tab_range ).

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


  METHOD zif_scabsrc_statements~reset.

    next = 0.

  ENDMETHOD.


  METHOD zif_scabsrc_statements~get_matching_groups_of_tokens.
    TYPES: BEGIN OF ty_word,
             from TYPE i,
             to   TYPE i,
           END OF ty_word,
           BEGIN OF ty_statement,
             words     TYPE STANDARD TABLE OF ty_word WITH EMPTY KEY,
             all_words TYPE string,
           END OF ty_statement.

    reset( ).
    DO zif_scabsrc_statements~count TIMES.
      DATA(statement) = get_next( ).

      DATA(tokens) = statement->get_matching_tokens( regex ).
      IF tokens->count > 0.
        APPEND tokens TO groups_of_tokens.
      ENDIF.

    ENDDO.

  ENDMETHOD.


ENDCLASS.
