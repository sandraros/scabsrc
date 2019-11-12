"! <p class="shorttext synchronized" lang="en"></p>
"!
INTERFACE zif_scabsrc_source_unit
  PUBLIC .


  METHODS get_invoking_unit
    RETURNING
      VALUE(source_unit) TYPE REF TO zif_scabsrc_source_unit .
  METHODS get_invoking_statement
    RETURNING
      VALUE(statement) TYPE REF TO zif_scabsrc_statement .
  METHODS get_statements
    RETURNING
      VALUE(statements) TYPE REF TO zif_scabsrc_statements .
  DATA slevel TYPE slevel READ-ONLY.
  DATA index TYPE sytabix READ-ONLY.
ENDINTERFACE.
