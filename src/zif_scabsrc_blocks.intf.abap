"! <p class="shorttext synchronized" lang="en"></p>
"!
INTERFACE zif_scabsrc_blocks
  PUBLIC .

  TYPES ty_statements TYPE STANDARD TABLE OF REF TO zif_scabsrc_statement WITH EMPTY KEY.
  METHODS reset .
  METHODS get_next
    RETURNING
      VALUE(block) TYPE REF TO zif_scabsrc_block .
  METHODS get_first_statement_each_block
    RETURNING
      VALUE(statements) TYPE ty_statements .
  DATA scabsrc TYPE REF TO zcl_scabsrc READ-ONLY.
  DATA count TYPE i READ-ONLY.
ENDINTERFACE.
