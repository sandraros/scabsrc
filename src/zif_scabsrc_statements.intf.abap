"! <p class="shorttext synchronized" lang="en"></p>
"!
INTERFACE zif_scabsrc_statements
  PUBLIC .

  TYPES ty_groups_of_tokens TYPE STANDARD TABLE OF REF TO zif_scabsrc_tokens WITH EMPTY KEY.

  METHODS reset.
  METHODS get_next
    RETURNING
      VALUE(statement) TYPE REF TO zif_scabsrc_statement .
  METHODS full_text_search
    IMPORTING
      !regex            TYPE csequence
      !limit_tokens     TYPE i
    RETURNING
      VALUE(statements) TYPE REF TO zif_scabsrc_statements .
  METHODS get_matching_groups_of_tokens
    IMPORTING
      regex                  TYPE csequence
    RETURNING
      VALUE(groups_of_tokens) TYPE ty_groups_of_tokens.

  DATA count TYPE i READ-ONLY.

ENDINTERFACE.
