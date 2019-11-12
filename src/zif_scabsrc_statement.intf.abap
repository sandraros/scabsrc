"! <p class="shorttext synchronized" lang="en"></p>
"!
INTERFACE zif_scabsrc_statement
  PUBLIC .

  "! <ul>
  "! <li>NATIVE_SQL (E) : statement between EXEC SQL and ENDEXEC</li>
  "! <li>INCLUDE (I) : INCLUDE program</li>
  "! <li>INCLUDE_MISS (J) : INCLUDE program where program does not exist, can occur only in connection with the addition WITH INCLUDES</li>
  "! <li>TYPE_POOLS (T) : TYPE-POOLS pool</li>
  "! <li>TYPE_POOLS_MISS (V) : TYPE-POOLS pool, pool does not exist</li>
  "! <li>TRMAC_CALL (R) : Call a macro from table TRMAC</li>
  "! <li>MACRO_CALL (D) : Call a macro internally defined with DEFINE</li>
  "! <li>MACRO_DEFINITION (M) : Macro definition between DEFINE and END-OF-DEFINITION</li>
  "! <li>COMPUTE_DIRECT (C) : COMPUTE statement, sometimes without COMPUTE as first token</li>
  "! <li>METHOD_DIRECT (A) : Method call in short form</li>
  "! <li>STANDARD (K) : Other ABAP/4 key word</li>
  "! <li>EMPTY (N) : Blank statement</li>
  "! <li>COMMENT (P) : Comment between statements</li>
  "! <li>COMMENT_IN_STMNT (S) : Comment within statements</li>
  "! <li>UNKNOWN (U) : non-blank statement</li>
  "! </ul>
  TYPES ty_type TYPE stmnt_type.
  TYPES ty_rng_type TYPE RANGE OF zif_scabsrc_statement=>ty_type.

  METHODS get_tokens
    RETURNING
      VALUE(tokens) TYPE REF TO zif_scabsrc_tokens .
  METHODS get_token
    IMPORTING
      position     TYPE i
    RETURNING
      VALUE(token) TYPE REF TO zif_scabsrc_token.
  METHODS get_block
    RETURNING
      VALUE(block) TYPE REF TO zif_scabsrc_block .
  METHODS get_source_unit
    RETURNING
      VALUE(source_unit) TYPE REF TO zif_scabsrc_source_unit .
  METHODS get_blocks
    IMPORTING
      type           TYPE zif_scabsrc_block=>ty_type OPTIONAL
      stmnt_type     TYPE zif_scabsrc_block=>ty_stmnt_type OPTIONAL
      rng_type       TYPE zif_scabsrc_block=>ty_rng_type OPTIONAL
      rng_stmnt_type TYPE zif_scabsrc_block=>ty_rng_stmnt_type OPTIONAL
    RETURNING
      VALUE(blocks)  TYPE REF TO zif_scabsrc_blocks .
  METHODS get_matching_tokens
    IMPORTING
      regex                  TYPE csequence
    RETURNING
      VALUE(tokens) TYPE REF TO zif_scabsrc_tokens.
  DATA scabsrc TYPE REF TO zcl_scabsrc READ-ONLY.
  DATA index TYPE sytabix READ-ONLY.
  DATA sstmnt TYPE sstmnt READ-ONLY.
  DATA tokens_count TYPE i READ-ONLY.

ENDINTERFACE.
