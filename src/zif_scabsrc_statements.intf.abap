"! <p class="shorttext synchronized" lang="en"></p>
"!
INTERFACE zif_scabsrc_statements
  PUBLIC .

  METHODS get_next
    RETURNING
      VALUE(statement) TYPE REF TO zif_scabsrc_statement .
  METHODS full_text_search
    IMPORTING
      !regex            TYPE csequence
    RETURNING
      VALUE(statements) TYPE REF TO zif_scabsrc_statements .
  DATA count TYPE i READ-ONLY.

ENDINTERFACE.
