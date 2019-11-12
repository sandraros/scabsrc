"! <p class="shorttext synchronized" lang="en"></p>
"!
INTERFACE zif_scabsrc_tokens
  PUBLIC .


  METHODS reset
    IMPORTING
      position TYPE i DEFAULT 1.
  METHODS get_next
    RETURNING
      VALUE(token) TYPE REF TO zif_scabsrc_token .
  METHODS get_token
    IMPORTING
      position     TYPE i
    RETURNING
      VALUE(token) TYPE REF TO zif_scabsrc_token .
  "! <p>Number of tokens</p>
  "! Example:
  "! <ul>
  "! <li>DATA(string) = VALUE string_table(</li>
  "! <li> &nbsp; &nbsp;FOR i = 1 THEN i + 1 WHILE i <= tokens->count</li>
  "! <li> &nbsp; &nbsp;( tokens->get_next( )->value ) ).</li>
  "! </ul>
  DATA count TYPE i READ-ONLY.
  DATA scabsrc TYPE REF TO zcl_scabsrc .
ENDINTERFACE.
