*"* use this source file for your ABAP unit test classes

CLASS ltc_main DEFINITION DEFERRED.
CLASS zcl_scabsrc_statement DEFINITION LOCAL FRIENDS
    ltc_main.

CLASS ltc_main DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS get_matching_tokens FOR TESTING.

ENDCLASS.

CLASS ltc_main IMPLEMENTATION.

  METHOD get_matching_tokens.

    DATA(scabsrc) = NEW zcl_scabsrc( it_srcline = VALUE #(
        ( |SELECT * FROM scarr UP TO 5 ROWS INTO TABLE @DATA(scarrs).| )
        ( |CALL FUNCTION 'A' DESTINATION 'B'.| )
        ( |CALL FUNCTION 'A' STARTING NEW TASK 'B'.| ) ) ).

    DATA(statements) = zcl_scabsrc_statements=>create( scabsrc ).

    DATA(statement) = statements->get_next( ).
    DATA(tokens) = statement->get_matching_tokens( regex = |UP\nTO\n(.+)\nROWS| ).
    cl_abap_unit_assert=>assert_equals( msg = '3rd token' exp = '5' act = tokens->get_token( 3 )->value ).

  ENDMETHOD.

ENDCLASS.
