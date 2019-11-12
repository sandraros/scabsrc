"! <p class="shorttext synchronized" lang="en"></p>
"!
class ZCL_SCABSRC definition
  public
  create public

  global friends ZCL_SCABSRC_TOKEN .

public section.

  data:
    lt_stokesx    TYPE TABLE OF stokesx read-only .
  data:
    lt_sstmnt     TYPE TABLE OF sstmnt read-only .
  data:
    lt_slevel     TYPE TABLE OF slevel read-only .
  data:
    lt_sstruc     TYPE TABLE OF sstruc read-only .
  data:
    lt_senhmt     TYPE TABLE OF senhmt read-only .

  methods CONSTRUCTOR
    importing
      !I_PROGRAM type SYREPID optional
      !IT_SRCLINE type STRING_TABLE optional .
  methods GET_TOKEN
    importing
      !INDEX type SYTABIX
    returning
      value(TOKEN) type ref to ZIF_SCABSRC_TOKEN .
  methods GET_TOKENS
    returning
      value(TOKENS) type ref to ZIF_SCABSRC_TOKENS .
  methods GET_STATEMENT
    importing
      !INDEX type SYTABIX
    returning
      value(STATEMENT) type ref to ZIF_SCABSRC_STATEMENT .
  methods GET_STATEMENTS
    importing
      !TYPE type ZIF_SCABSRC_STATEMENT=>TY_TYPE optional
      !RNG_TYPE type ZIF_SCABSRC_STATEMENT=>TY_RNG_TYPE optional
    returning
      value(STATEMENTS) type ref to ZIF_SCABSRC_STATEMENTS .
  methods GET_BLOCK
    importing
      !INDEX type SYTABIX
    returning
      value(BLOCK) type ref to ZIF_SCABSRC_BLOCK .
  methods GET_BLOCKS
    importing
      !TYPE type ZIF_SCABSRC_BLOCK=>TY_TYPE optional
      !STMNT_TYPE type ZIF_SCABSRC_BLOCK=>TY_STMNT_TYPE optional
      !RNG_TYPE type ZIF_SCABSRC_BLOCK=>TY_RNG_TYPE optional
      !RNG_STMNT_TYPE type ZIF_SCABSRC_BLOCK=>TY_RNG_STMNT_TYPE optional
    returning
      value(BLOCKS) type ref to ZIF_SCABSRC_BLOCKS .
  methods GET_SOURCE_UNIT
    importing
      !INDEX type SYTABIX
    returning
      value(SOURCE_UNIT) type ref to ZIF_SCABSRC_SOURCE_UNIT .
  methods GET_SOURCE_UNITS
    returning
      value(SOURCE_UNITS) type ref to ZIF_SCABSRC_SOURCE_UNITS .
  methods GET_TOKENS_OF_LINE
    importing
      !INCLUDE type SYREPID
      !LINE type NUMERIC
    returning
      value(TOKENS) type ref to ZIF_SCABSRC_TOKENS .
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



CLASS ZCL_SCABSRC IMPLEMENTATION.


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


  METHOD get_source_units.

    CREATE OBJECT source_units TYPE zcl_scabsrc_source_units
      EXPORTING
        scabsrc = me.

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


  METHOD get_statements.

    statements = zcl_scabsrc_statements=>create(
        scabsrc = me ).

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


  METHOD get_tokens.

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
ENDCLASS.
