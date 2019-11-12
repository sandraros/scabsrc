"! <p class="shorttext synchronized" lang="en"></p>
"!
CLASS zcl_scabsrc_statement DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_scabsrc_statement .

    ALIASES sstmnt
      FOR zif_scabsrc_statement~sstmnt .
    ALIASES get_block
      FOR zif_scabsrc_statement~get_block .
    ALIASES get_blocks
      FOR zif_scabsrc_statement~get_blocks .
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

ENDCLASS.



CLASS zcl_scabsrc_statement IMPLEMENTATION.


  METHOD constructor.

    me->zif_scabsrc_statement~index = index.
    me->zif_scabsrc_statement~scabsrc = scabsrc.
    me->zif_scabsrc_statement~sstmnt = scabsrc->lt_sstmnt[ index ].
    me->zif_scabsrc_statement~tokens_count = zif_scabsrc_statement~sstmnt-to - zif_scabsrc_statement~sstmnt-from + 1.

  ENDMETHOD.


  METHOD zif_scabsrc_statement~get_block.

    CREATE OBJECT block TYPE zcl_scabsrc_block
      EXPORTING
        scabsrc = zif_scabsrc_statement~scabsrc
        index   = zif_scabsrc_statement~sstmnt-struc.

  ENDMETHOD.


  METHOD zif_scabsrc_statement~get_blocks.

    blocks = zcl_scabsrc_blocks=>create_for_statement(
        statement      = me
        type           = type
        stmnt_type     = stmnt_type
        rng_type       = rng_type
        rng_stmnt_type = rng_stmnt_type ).

  ENDMETHOD.


  METHOD zif_scabsrc_statement~get_matching_tokens.
    TYPES: BEGIN OF ty_word,
             from TYPE i,
             to   TYPE i,
           END OF ty_word,
           BEGIN OF ty_statement,
             words     TYPE STANDARD TABLE OF ty_word WITH EMPTY KEY,
             all_words TYPE string,
           END OF ty_statement.

    DATA(statement) = REDUCE ty_statement(
        INIT aux TYPE ty_statement
        FOR <token> IN zif_scabsrc_statement~scabsrc->lt_stokesx
            FROM zif_scabsrc_statement~sstmnt-from
            TO zif_scabsrc_statement~sstmnt-to
        NEXT aux-words = VALUE #(
                LET prev_to = COND #( WHEN aux-words IS INITIAL THEN -2 ELSE aux-words[ lines( aux-words ) ]-to ) IN
                BASE aux-words (
                from = prev_to + 2
                to   = prev_to + 1 + strlen( <token>-str ) ) )
             aux-all_words = |{ aux-all_words }{ <token>-str }\n| ).

    FIND FIRST OCCURRENCE OF REGEX regex IN statement-all_words RESULTS DATA(match).
    CHECK sy-subrc = 0.
    READ TABLE statement-words WITH KEY from = match-offset BINARY SEARCH TRANSPORTING NO FIELDS.
    DATA(first_word_number) = COND i( WHEN sy-subrc = 0 THEN sy-tabix ELSE sy-tabix - 1 ).
    READ TABLE statement-words WITH KEY from = match-offset + match-length - 1 BINARY SEARCH TRANSPORTING NO FIELDS.
    DATA(last_word_number) = COND i( WHEN sy-subrc = 0 THEN sy-tabix ELSE sy-tabix - 1 ).
    tokens = NEW zcl_scabsrc_tokens(
        scabsrc = zif_scabsrc_statement~scabsrc
        from    = zif_scabsrc_statement~sstmnt-from + first_word_number - 1
        to      = zif_scabsrc_statement~sstmnt-from + last_word_number - 1 ).

  ENDMETHOD.


  METHOD zif_scabsrc_statement~get_source_unit.

    CREATE OBJECT source_unit TYPE zcl_scabsrc_source_unit
      EXPORTING
        scabsrc = zif_scabsrc_statement~scabsrc
        index   = zif_scabsrc_statement~sstmnt-level.

  ENDMETHOD.


  METHOD zif_scabsrc_statement~get_token.

    DATA(index_token) = zif_scabsrc_statement~sstmnt-from + position - 1.
    IF index_token BETWEEN zif_scabsrc_statement~sstmnt-from AND zif_scabsrc_statement~sstmnt-to.
      CREATE OBJECT token TYPE zcl_scabsrc_token
        EXPORTING
          scabsrc = zif_scabsrc_statement~scabsrc
          index   = index_token.
    ENDIF.

  ENDMETHOD.


  METHOD zif_scabsrc_statement~get_tokens.

    CREATE OBJECT tokens TYPE zcl_scabsrc_tokens
      EXPORTING
        scabsrc = zif_scabsrc_statement~scabsrc
        from    = zif_scabsrc_statement~sstmnt-from
        to      = zif_scabsrc_statement~sstmnt-to.

  ENDMETHOD.


ENDCLASS.
