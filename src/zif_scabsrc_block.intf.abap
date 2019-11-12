"! <p class="shorttext synchronized" lang="en"></p>
"!
INTERFACE zif_scabsrc_block
  PUBLIC .

  "! Constants are in global structure <strong>SCAN_STRUC_TYPE</strong> (type pool SCAN)
  "! <ul>
  "! <li>ALTERNATION (A) : Case distinction (IF, CASE, AT, etc.)</li>
  "! <li>CONDITION (C) : Condition in a case distinction (WHEN, THEN, ELSE, ELSEIF, etc.)</li>
  "! <li>DECLARATION (D) : Structured declaration (anything which contains BEGIN OF and END OF)</li>
  "! <li>EVENT (E) : report event (AT SELECTION-SCREEN, START-OF-SELECTION, etc.)</li>
  "! <li>ITERATION (I) : Iteration (DO, LOOP AT, WHILE, etc.)</li>
  "! <li>JUMP (J) : exit or call (EXIT, RETURN, SUBMIT, etc.)</li>
  "! <li>CLASS (L) : CLASS DEFINITION, PUBLIC SECTION, etc.</li>
  "! <li>MACRO (M) : DEFINE, EXEC SQL</li>
  "! <li>PROG (P) : Beginning of the source code (REPORT, etc.)</li>
  "! <li>ROUTINE (R) : procedure (FORM, FUNCTION, METHOD, MODULE)</li>
  "! <li>SEQUENCE (S) : all other statements</li>
  "! </ul>
  TYPES ty_type TYPE sstruc-type.
  "! Constants are in global structure <strong>SCAN_STRUC_STMNT_TYPE</strong> (type pool SCAN):
  "! <ul>
  "! <li>Alternations: IF, CASE, ON, AT, TRY</li>
  "! <li>Conditions: WHEN, THEN, ELSE, ELSEIF, CATCH, CLEANUP</li>
  "! <li>Declaration of data Objects: DATA, DATA_COMMON, STATICS, CONSTANTS, TYPES, SELECTION_SCREEN</li>
  "! <li>Events: INITIALIZATION, START_OF_SELECTION, END_OF_SELECTION, START_OF_EDITING, END_OF_EDITING, TOP_OF_PAGE, TOP_OF_PAGE_DURING, END_OF_PAGE, SYSTEM_EXIT, AT_USER_COMMAND, AT_LINE_SELECTION, AT_SELECTION_SCREEN, AT_PF_I</li>
  "! <li>Iterations: DO, WHILE, LOOP, PROVIDE, SELECT</li>
  "! <li>Jumps: CHECK, CONTINUE, EXIT, RETURN, REJECT, STOP, LEAVE, RAISE, CALL, SUBMIT</li>
  "! <li>Local object types: CLASS DEFINITION, CLASS IMPLEMENTATION, INTERFACE, PUBLIC_SECTION, PACKAGE_SECTION, PROTECTED_SECTION, PRIVATE_SECTION</li>
  "! <li>Macros: DEFINE, EXEC</li>
  "! <li>Procedures (R): MODULE, FUNCTION, FORM, METHOD</li>
  "! <li>Program introductions: PROGRAM, REPORT, FUNCTION_POOL, TYPE_POOL</li>
  "! <li>SEQUENCE</li>
  "! </ul>
  TYPES ty_stmnt_type TYPE sstruc-stmnt_type.
  TYPES: ty_rng_type       TYPE RANGE OF ty_type,
         ty_rng_stmnt_type TYPE RANGE OF ty_stmnt_type.

  METHODS get_statements
    RETURNING
      VALUE(statements) TYPE REF TO zif_scabsrc_statements .
  METHODS get_child_blocks
    RETURNING
      VALUE(blocks) TYPE REF TO zif_scabsrc_blocks .
  METHODS get_parent_block
    RETURNING
      VALUE(block) TYPE REF TO zif_scabsrc_block .
*  METHODS get_all_fields
*    RETURNING
*      VALUE(block) TYPE sstruc .
*  METHODS get_index
*    RETURNING
*      VALUE(index) TYPE sytabix .
  METHODS get_type
    RETURNING
      VALUE(type) TYPE ty_type .
  METHODS get_stmnt_type
    RETURNING
      VALUE(stmnt_type) TYPE ty_stmnt_type.
  METHODS get_first_statement
    RETURNING
      VALUE(statement) TYPE REF TO zif_scabsrc_statement .
  DATA scabsrc TYPE REF TO zcl_scabsrc READ-ONLY.
  DATA sstruc TYPE sstruc READ-ONLY.
  DATA index TYPE sytabix READ-ONLY.
ENDINTERFACE.
