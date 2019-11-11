class ZCL_SCABSRC definition
  public
  create public
  GLOBAL FRIENDS zCL_SCABSRC_TOKEN.

public section.

  data:
    LT_STOKESX    TYPE TABLE OF STOKESX .
  data:
    LT_SSTMNT     TYPE TABLE OF SSTMNT .
  data:
    LT_SLEVEL     TYPE TABLE OF SLEVEL .
  data:
    LT_SSTRUC     TYPE TABLE OF SSTRUC .
  data:
    LT_SENHMT     TYPE TABLE OF SENHMT .

  methods CONSTRUCTOR
    importing
      !I_PROGRAM type SYREPID optional
      !IT_SRCLINE type STRING_TABLE optional .
  methods GET_TOKEN
    importing
      !INDEX type SYTABIX
    returning
      value(TOKEN) type ref to ZIF_SCABSRC_TOKEN .
  methods GET_STATEMENT
    importing
      !INDEX type SYTABIX
    returning
      value(STATEMENT) type ref to ZIF_SCABSRC_STATEMENT .
  methods GET_BLOCK
    importing
      !INDEX type SYTABIX
    returning
      value(BLOCK) type ref to ZIF_SCABSRC_BLOCK .
  methods GET_BLOCKS
    importing
      !TYPE type SSTRUC-TYPE optional
      !STMNT_TYPE type SSTRUC-STMNT_TYPE optional
    returning
      value(BLOCKS) type ref to ZIF_SCABSRC_BLOCKS .
  methods GET_SOURCE_UNIT
    importing
      !INDEX type SYTABIX
    returning
      value(SOURCE_UNIT) type ref to ZIF_SCABSRC_SOURCE_UNIT .
  methods GET_TOKENS_OF_LINE
    importing
      !INCLUDE type SYREPID
      !LINE type NUMERIC
    returning
      value(TOKENS) type ref to ZIF_SCABSRC_TOKENS .
protected section.
private section.

  types:
    BEGIN OF TY_IS_STATEMENT_BY_FROM,
              INDEX     TYPE SYTABIX,
              FROM      TYPE SSTMNT-FROM,
            END OF TY_IS_STATEMENT_BY_FROM .

  data:
    AIT_STATEMENT_BY_FROM TYPE SORTED TABLE OF TY_IS_STATEMENT_BY_FROM WITH UNIQUE KEY FROM .

  methods CALCULATE_STATEMENT_BY_FROM .
ENDCLASS.



CLASS ZCL_SCABSRC IMPLEMENTATION.


  method CALCULATE_STATEMENT_BY_FROM.

    FIELD-SYMBOLS <LS_SSTMNT> TYPE SSTMNT.
    DATA LS_STATEMENT_BY_FROM TYPE TY_IS_STATEMENT_BY_FROM.

    LOOP AT LT_SSTMNT ASSIGNING <LS_SSTMNT>.
      LS_STATEMENT_BY_FROM-INDEX = SY-TABIX.
      LS_STATEMENT_BY_FROM-FROM  = <LS_SSTMNT>-FROM. "index token
      INSERT LS_STATEMENT_BY_FROM INTO TABLE AIT_STATEMENT_BY_FROM.
    ENDLOOP.

  endmethod.


  method CONSTRUCTOR.

    DATA LT_SRCLINE     TYPE TABLE OF STRING.

    FIELD-SYMBOLS <LT_SRCLINE> TYPE STRING_TABLE.
    IF IT_SRCLINE IS INITIAL AND I_PROGRAM IS INITIAL.
      RETURN.
    ENDIF.
    IF IT_SRCLINE IS INITIAL.
      READ REPORT I_PROGRAM INTO LT_SRCLINE.
      ASSIGN LT_SRCLINE TO <LT_SRCLINE>.
    ELSE.
      ASSIGN IT_SRCLINE TO <LT_SRCLINE>.
    ENDIF.
    SCAN ABAP-SOURCE <LT_SRCLINE>
          TOKENS INTO LT_STOKESX
          STATEMENTS INTO LT_SSTMNT
          STRUCTURES INTO LT_SSTRUC
          INCLUDE PROGRAM FROM I_PROGRAM
          FRAME PROGRAM FROM I_PROGRAM
          WITH TYPE-POOLS
          WITH LIST TOKENIZATION
          PRESERVING IDENTIFIER ESCAPING
*     WITH EXPLICIT ENHANCEMENTS "interdit avec WITH ANALYSIS
          ENHANCEMENTS INTO LT_SENHMT
          WITH ANALYSIS
          WITH COMMENTS
          WITH INCLUDES
          LEVELS INTO LT_SLEVEL.

  endmethod.


  method GET_BLOCK.

    READ TABLE LT_SSTRUC INDEX INDEX TRANSPORTING NO FIELDS.
    IF SY-SUBRC = 0.
      CREATE OBJECT BLOCK TYPE ZCL_SCABSRC_BLOCK
        EXPORTING
          SCABSRC = ME
          INDEX   = INDEX.
    ENDIF.

  endmethod.


  method GET_BLOCKS.

    DATA LO_BLOCK TYPE REF TO ZIF_SCABSRC_BLOCK.
    DATA LT_RNG_TYPE        TYPE RANGE OF SSTRUC-TYPE.
    DATA LS_RNG_TYPE        LIKE LINE OF LT_RNG_TYPE.
    DATA LT_RNG_STMNT_TYPE  TYPE RANGE OF SSTRUC-STMNT_TYPE.
    DATA LS_RNG_STMNT_TYPE  LIKE LINE OF LT_RNG_STMNT_TYPE.
    FIELD-SYMBOLS <LS_SSTRUC> TYPE SSTRUC.
    DATA INDEX              TYPE SYINDEX.
    DATA PASSED_THROUGH     TYPE ABAP_BOOL.

    IF TYPE IS NOT SUPPLIED AND STMNT_TYPE IS NOT SUPPLIED.
      CREATE OBJECT BLOCKS TYPE ZCL_SCABSRC_BLOCKS
        EXPORTING
          SCABSRC = ME
          FROM    = 1
          TO      = LINES( LT_SSTRUC ).
    ELSE.
      IF TYPE IS SUPPLIED.
        CLEAR LS_RNG_TYPE.
        LS_RNG_TYPE-SIGN    = 'I'.
        LS_RNG_TYPE-OPTION  = 'EQ'.
        LS_RNG_TYPE-LOW     = TYPE.
        APPEND LS_RNG_TYPE TO LT_RNG_TYPE.
      ENDIF.
      IF STMNT_TYPE IS SUPPLIED.
        CLEAR LS_RNG_STMNT_TYPE.
        LS_RNG_STMNT_TYPE-SIGN    = 'I'.
        LS_RNG_STMNT_TYPE-OPTION  = 'EQ'.
        LS_RNG_STMNT_TYPE-LOW     = STMNT_TYPE.
        APPEND LS_RNG_STMNT_TYPE TO LT_RNG_STMNT_TYPE.
      ENDIF.
      LOOP AT LT_SSTRUC ASSIGNING <LS_SSTRUC>
            WHERE TYPE       IN LT_RNG_TYPE
              AND STMNT_TYPE IN LT_RNG_STMNT_TYPE.
        INDEX = SY-TABIX.
        IF PASSED_THROUGH IS INITIAL.
          CREATE OBJECT BLOCKS TYPE ZCL_SCABSRC_BLOCKS
            EXPORTING
              SCABSRC = ME.
          PASSED_THROUGH = ABAP_TRUE.
        ENDIF.
        BLOCKS->ADD_BLOCK( INDEX ).
      ENDLOOP.
    ENDIF.

  endmethod.


  method GET_SOURCE_UNIT.

    READ TABLE LT_SLEVEL INDEX INDEX TRANSPORTING NO FIELDS.
    IF SY-SUBRC = 0.
      CREATE OBJECT SOURCE_UNIT TYPE ZCL_SCABSRC_SOURCE_UNIT
        EXPORTING
          SCABSRC = ME
          INDEX   = INDEX.
    ENDIF.

  endmethod.


  method GET_STATEMENT.

    READ TABLE LT_SSTMNT INDEX INDEX TRANSPORTING NO FIELDS.
    IF SY-SUBRC = 0.
      CREATE OBJECT STATEMENT TYPE ZCL_SCABSRC_STATEMENT
        EXPORTING
          SCABSRC = ME
          INDEX   = INDEX.
    ENDIF.

  endmethod.


  method GET_TOKEN.

    READ TABLE LT_STOKESX INDEX INDEX TRANSPORTING NO FIELDS.
    IF SY-SUBRC = 0.
      CREATE OBJECT TOKEN TYPE ZCL_SCABSRC_TOKEN
        EXPORTING
          SCABSRC = ME
          INDEX   = INDEX.
    ENDIF.

  endmethod.


  method GET_TOKENS_OF_LINE.

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

  endmethod.
ENDCLASS.
