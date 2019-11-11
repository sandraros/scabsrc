"! <p class="shorttext synchronized" lang="en"></p>
"!
CLASS zcl_scabsrc DEFINITION
  PUBLIC
  CREATE PUBLIC
  GLOBAL FRIENDS zcl_scabsrc_token.

  PUBLIC SECTION.

    DATA:
      lt_stokesx    TYPE TABLE OF stokesx .
    DATA:
      lt_sstmnt     TYPE TABLE OF sstmnt .
    DATA:
      lt_slevel     TYPE TABLE OF slevel .
    DATA:
      lt_sstruc     TYPE TABLE OF sstruc .
    DATA:
      lt_senhmt     TYPE TABLE OF senhmt .

    METHODS constructor
      IMPORTING
        !i_program  TYPE syrepid OPTIONAL
        !it_srcline TYPE string_table OPTIONAL .
    METHODS get_token
      IMPORTING
        !index       TYPE sytabix
      RETURNING
        VALUE(token) TYPE REF TO zif_scabsrc_token .
    METHODS get_tokens
      RETURNING
        VALUE(tokens) TYPE REF TO zif_scabsrc_tokens .
    METHODS get_statement
      IMPORTING
        !index           TYPE sytabix
      RETURNING
        VALUE(statement) TYPE REF TO zif_scabsrc_statement .
    METHODS get_statements
      IMPORTING
        type              TYPE zif_scabsrc_statement=>ty_type OPTIONAL
        rng_type          TYPE zif_scabsrc_statement=>ty_rng_type OPTIONAL
      RETURNING
        VALUE(statements) TYPE REF TO zif_scabsrc_statements .
    METHODS get_block
      IMPORTING
        !index       TYPE sytabix
      RETURNING
        VALUE(block) TYPE REF TO zif_scabsrc_block .
    METHODS get_blocks
      IMPORTING
        type           TYPE zif_scabsrc_block=>ty_type OPTIONAL
        stmnt_type     TYPE zif_scabsrc_block=>ty_stmnt_type OPTIONAL
        rng_type       TYPE zif_scabsrc_block=>ty_rng_type OPTIONAL
        rng_stmnt_type TYPE zif_scabsrc_block=>ty_rng_stmnt_type OPTIONAL
      RETURNING
        VALUE(blocks)  TYPE REF TO zif_scabsrc_blocks .
    METHODS get_source_unit
      IMPORTING
        !index             TYPE sytabix
      RETURNING
        VALUE(source_unit) TYPE REF TO zif_scabsrc_source_unit .
    METHODS get_source_units
      RETURNING
        VALUE(source_units) TYPE REF TO zif_scabsrc_source_units .
    METHODS get_tokens_of_line
      IMPORTING
        !include      TYPE syrepid
        !line         TYPE numeric
      RETURNING
        VALUE(tokens) TYPE REF TO zif_scabsrc_tokens .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_is_statement_by_from,
        index TYPE sytabix,
        from  TYPE sstmnt-from,
      END OF ty_is_statement_by_from .

    DATA:
      ait_statement_by_from TYPE SORTED TABLE OF ty_is_statement_by_from WITH UNIQUE KEY from .

    METHODS calculate_statement_by_from .
ENDCLASS.



CLASS zcl_scabsrc IMPLEMENTATION.


  METHOD calculate_statement_by_from.

    FIELD-SYMBOLS <ls_sstmnt> TYPE sstmnt.
    DATA ls_statement_by_from TYPE ty_is_statement_by_from.

    LOOP AT lt_sstmnt ASSIGNING <ls_sstmnt>.
      ls_statement_by_from-index = sy-tabix.
      ls_statement_by_from-from  = <ls_sstmnt>-from. "index token
      INSERT ls_statement_by_from INTO TABLE ait_statement_by_from.
    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    DATA lt_srcline     TYPE TABLE OF string.

    FIELD-SYMBOLS <lt_srcline> TYPE string_table.
    IF it_srcline IS INITIAL AND i_program IS INITIAL.
      RETURN.
    ENDIF.
    IF it_srcline IS INITIAL.
      READ REPORT i_program INTO lt_srcline.
      ASSIGN lt_srcline TO <lt_srcline>.
    ELSE.
      ASSIGN it_srcline TO <lt_srcline>.
    ENDIF.
    SCAN ABAP-SOURCE <lt_srcline>
          TOKENS INTO lt_stokesx
          STATEMENTS INTO lt_sstmnt
          STRUCTURES INTO lt_sstruc
          INCLUDE PROGRAM FROM i_program
          FRAME PROGRAM FROM i_program
          WITH TYPE-POOLS
          WITH LIST TOKENIZATION
          PRESERVING IDENTIFIER ESCAPING
*     WITH EXPLICIT ENHANCEMENTS "interdit avec WITH ANALYSIS
          ENHANCEMENTS INTO lt_senhmt
          WITH ANALYSIS
          WITH COMMENTS
          WITH INCLUDES
          LEVELS INTO lt_slevel.

  ENDMETHOD.


  METHOD get_block.

    READ TABLE lt_sstruc INDEX index TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      CREATE OBJECT block TYPE zcl_scabsrc_block
        EXPORTING
          scabsrc = me
          index   = index.
    ENDIF.

  ENDMETHOD.


  METHOD get_blocks.

    blocks = zcl_scabsrc_blocks=>create(
        scabsrc        = me
        type           = type
        stmnt_type     = stmnt_type
        rng_type       = rng_type
        rng_stmnt_type = rng_stmnt_type ).

*    DATA lo_block TYPE REF TO zif_scabsrc_block.
*    DATA lt_rng_type        TYPE RANGE OF sstruc-type.
*    DATA ls_rng_type        LIKE LINE OF lt_rng_type.
*    DATA lt_rng_stmnt_type  TYPE RANGE OF sstruc-stmnt_type.
*    DATA ls_rng_stmnt_type  LIKE LINE OF lt_rng_stmnt_type.
*    FIELD-SYMBOLS <ls_sstruc> TYPE sstruc.
*    DATA index              TYPE syindex.
*    DATA passed_through     TYPE abap_bool.
*
*    IF type IS NOT SUPPLIED AND stmnt_type IS NOT SUPPLIED.
*      CREATE OBJECT blocks TYPE zcl_scabsrc_blocks
*        EXPORTING
*          scabsrc = me
*          from    = 1
*          to      = lines( lt_sstruc ).
*    ELSE.
*      IF type IS SUPPLIED.
*        CLEAR ls_rng_type.
*        ls_rng_type-sign    = 'I'.
*        ls_rng_type-option  = 'EQ'.
*        ls_rng_type-low     = type.
*        APPEND ls_rng_type TO lt_rng_type.
*      ENDIF.
*      IF stmnt_type IS SUPPLIED.
*        CLEAR ls_rng_stmnt_type.
*        ls_rng_stmnt_type-sign    = 'I'.
*        ls_rng_stmnt_type-option  = 'EQ'.
*        ls_rng_stmnt_type-low     = stmnt_type.
*        APPEND ls_rng_stmnt_type TO lt_rng_stmnt_type.
*      ENDIF.
*      LOOP AT lt_sstruc ASSIGNING <ls_sstruc>
*            WHERE type       IN lt_rng_type
*              AND stmnt_type IN lt_rng_stmnt_type.
*        index = sy-tabix.
*        IF passed_through IS INITIAL.
*          CREATE OBJECT blocks TYPE zcl_scabsrc_blocks
*            EXPORTING
*              scabsrc = me.
*          passed_through = abap_true.
*        ENDIF.
*        blocks->add_block( index ).
*      ENDLOOP.
*    ENDIF.

  ENDMETHOD.


  METHOD get_source_unit.

    READ TABLE lt_slevel INDEX index TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      CREATE OBJECT source_unit TYPE zcl_scabsrc_source_unit
        EXPORTING
          scabsrc = me
          index   = index.
    ENDIF.

  ENDMETHOD.


  METHOD get_statement.

    READ TABLE lt_sstmnt INDEX index TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      CREATE OBJECT statement TYPE zcl_scabsrc_statement
        EXPORTING
          scabsrc = me
          index   = index.
    ENDIF.

  ENDMETHOD.


  METHOD get_token.

    READ TABLE lt_stokesx INDEX index TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      CREATE OBJECT token TYPE zcl_scabsrc_token
        EXPORTING
          scabsrc = me
          index   = index.
    ENDIF.

  ENDMETHOD.


  METHOD get_tokens_of_line.

    RAISE EXCEPTION TYPE zcx_scabsrc_todo.
*    FIELD-SYMBOLS <ls_source_unit2> TYPE ty_us_source_unit.
*    FIELD-SYMBOLS <ls_source_unit> TYPE ty_is_source_unit.
*    FIELD-SYMBOLS <ls_token> TYPE ty_us_token.
*    FIELD-SYMBOLS <ls_statement> TYPE ty_is_statement.
*    READ TABLE aut_source_unit ASSIGNING <ls_source_unit2> WITH KEY name = include.
*    IF sy-subrc = 0.
*      READ TABLE ait_source_unit ASSIGNING <ls_source_unit> INDEX <ls_source_unit2>-id-value.
*      IF sy-subrc = 0 AND <ls_source_unit>-from <= <ls_source_unit>-to.
*        LOOP AT ait_statement FROM <ls_source_unit>-from TO <ls_source_unit>-to ASSIGNING <ls_statement>.
*          IF <ls_statement>-from <= <ls_statement>-to.
*            LOOP AT aut_token FROM <ls_statement>-from TO <ls_statement>-to ASSIGNING <ls_token>
*                  WHERE row = line.
**              APPEND <ls_token> TO result.
*            ENDLOOP.
*          ENDIF.
*        ENDLOOP.
*      ENDIF.
*    ENDIF.

  ENDMETHOD.

  METHOD get_statements.

    statements = zcl_scabsrc_statements=>create(
        scabsrc = me ).

  ENDMETHOD.

  METHOD get_source_units.

    CREATE OBJECT source_units TYPE zcl_scabsrc_source_units
      EXPORTING
        scabsrc = me.

  ENDMETHOD.

  METHOD get_tokens.

  ENDMETHOD.

ENDCLASS.
