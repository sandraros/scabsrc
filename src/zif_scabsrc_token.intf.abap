"! <p class="shorttext synchronized" lang="en"></p>
"!
INTERFACE zif_scabsrc_token
  PUBLIC .


  METHODS get_statement
    RETURNING
      VALUE(statement) TYPE REF TO zif_scabsrc_statement .
  METHODS get_all_fields
    RETURNING
      VALUE(token) TYPE stokesx .
  DATA value TYPE stokesx-str READ-ONLY.

ENDINTERFACE.
