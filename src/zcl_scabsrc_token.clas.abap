"! <p class="shorttext synchronized" lang="en"></p>
"!
CLASS zcl_scabsrc_token DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_scabsrc_token .

    ALIASES get_all_fields
      FOR zif_scabsrc_token~get_all_fields .
    ALIASES get_statement
      FOR zif_scabsrc_token~get_statement .
    ALIASES value
      FOR zif_scabsrc_token~value .

    METHODS constructor
      IMPORTING
        !scabsrc TYPE REF TO zcl_scabsrc
        !index   TYPE i .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA index TYPE sytabix .
    DATA scabsrc TYPE REF TO zcl_scabsrc .
ENDCLASS.



CLASS zcl_scabsrc_token IMPLEMENTATION.


  METHOD constructor.

    me->index = index.
    me->scabsrc = scabsrc.
    value = scabsrc->lt_stokesx[ index ]-str.

  ENDMETHOD.


  METHOD zif_scabsrc_token~get_all_fields.

    READ TABLE scabsrc->lt_stokesx INDEX index INTO token.

  ENDMETHOD.


  METHOD zif_scabsrc_token~get_statement.

    FIELD-SYMBOLS <ls_sstruc> TYPE sstruc.
    FIELD-SYMBOLS <ls_statement> TYPE zcl_scabsrc=>ty_is_statement_by_from.

    IF scabsrc->ait_statement_by_from[] IS INITIAL.
      scabsrc->calculate_statement_by_from( ).
    ENDIF.
    READ TABLE scabsrc->ait_statement_by_from ASSIGNING <ls_statement>
          WITH KEY from = index
          BINARY SEARCH.
    IF sy-subrc = 4.
      READ TABLE scabsrc->ait_statement_by_from ASSIGNING <ls_statement> INDEX sy-tabix.
    ELSEIF sy-subrc = 8.
      READ TABLE scabsrc->ait_statement_by_from ASSIGNING <ls_statement> INDEX sy-tfill.
    ENDIF.
    IF sy-subrc = 0.
      CREATE OBJECT statement TYPE zcl_scabsrc_statement
        EXPORTING
          scabsrc = scabsrc
          index   = <ls_statement>-index.
    ENDIF.

  ENDMETHOD.


ENDCLASS.
