*"* use this source file for your ABAP unit test classes

CLASS ltc_main DEFINITION DEFERRED.
CLASS zcl_scabsrc_statements DEFINITION LOCAL FRIENDS
    ltc_main.

CLASS ltc_main DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS create FOR TESTING.
    METHODS full_text_search FOR TESTING.
    METHODS get_matching_groups_of_tokens FOR TESTING.

ENDCLASS.

CLASS ltc_main IMPLEMENTATION.

  METHOD create.
    DATA(scabsrc) = NEW zcl_scabsrc( it_srcline = VALUE #(
        ( |  REPORT  .| )
        ( |  CALL  'C PROGRAM'  .| ) ) ).
    DATA(statements) = zcl_scabsrc_statements=>create( scabsrc ).
    DATA(list_of_statements) = VALUE string_table(
        FOR i = 1 THEN i + 1 WHILE i <= statements->count
        LET tokens = statements->get_next( )->get_tokens( ) IN
        ( REDUCE #(
            INIT str = ``
            FOR j = 1 THEN j + 1 WHILE j <= tokens->count
            LET start = COND #( WHEN j > 1 THEN ` ` )
                end = COND #( WHEN j = tokens->count THEN `.` ) IN
            NEXT str = str && start && tokens->get_next( )->value && end ) ) ).
    cl_abap_unit_assert=>assert_equals( act = list_of_statements exp = VALUE string_table(
        ( |REPORT.| )
        ( |CALL 'C PROGRAM'.| ) ) ).
  ENDMETHOD.

  METHOD full_text_search.
    DATA(scabsrc) = NEW zcl_scabsrc( it_srcline = VALUE #(
        ( |REPORT.| )
        ( |CALL 'C PROGRAM'.| )
        ( |CALL TRANSACTION '1'.| )
        ( |CALL FUNCTION 'A'.| )
        ( |CALL TRANSACTION '2'.| )
        ( |  CALL  FUNCTION 'B'.| )
        ( |CALL FUNCTIONS 'C'.| ) ) ).
    DATA(statements) = scabsrc->get_statements( )->full_text_search( regex = 'CALL\nFUNCTION' limit_tokens = 2 ).
    cl_abap_unit_assert=>assert_equals( msg = 'number of matching statements' act = statements->count exp = 2 ).
    DATA(string) = VALUE string( ).
    DO statements->count TIMES.
      DATA(tokens) = statements->get_next( )->get_tokens( ).
      string = string
          && REDUCE #(
              INIT str = ``
              FOR i = 1 THEN i + 1 WHILE i <= tokens->count
              NEXT str = str && tokens->get_next( )->value && ` ` )
          && `.`.
    ENDDO.
    cl_abap_unit_assert=>assert_equals( exp = `CALL FUNCTION 'A' .CALL FUNCTION 'B' .` act = string ).
  ENDMETHOD.

  METHOD get_matching_groups_of_tokens.
    DATA: groups_of_tokens TYPE zif_scabsrc_statements=>ty_groups_of_tokens.

    DATA(scabsrc) = NEW zcl_scabsrc( it_srcline = VALUE #(
        ( |SELECT * FROM scarr UP TO 5 ROWS INTO TABLE @DATA(scarrs).| )
        ( |CALL FUNCTION 'A' DESTINATION 'B'.| )
        ( |CALL FUNCTION 'A' STARTING NEW TASK 'B'.| ) ) ).
    DATA(statements) = zcl_scabsrc_statements=>create( scabsrc ).

    groups_of_tokens = statements->get_matching_groups_of_tokens( regex = |DESTINATION\n(.+)\n| && '|' && |STARTING\nNEW\nTASK\n(.+)\n| ).
    cl_abap_unit_assert=>assert_equals( msg = 'number of groups of tokens' exp = 2 act = lines( groups_of_tokens ) ).
    cl_abap_unit_assert=>assert_equals( msg = 'number of tokens of 1st group' exp = 2 act = groups_of_tokens[ 1 ]->count ).
    cl_abap_unit_assert=>assert_equals( msg = '1st token of 1st group' exp = |DESTINATION| act = groups_of_tokens[ 1 ]->get_token( 1 )->value ).
    cl_abap_unit_assert=>assert_equals( msg = '2nd token of 1st group' exp = |'B'| act = groups_of_tokens[ 1 ]->get_token( 2 )->value ).
    cl_abap_unit_assert=>assert_equals( msg = 'number of tokens of 2nd group' exp = 4 act = groups_of_tokens[ 2 ]->count ).
    cl_abap_unit_assert=>assert_equals( msg = '1st token of 2nd group' exp = |STARTING| act = groups_of_tokens[ 2 ]->get_token( 1 )->value ).
    cl_abap_unit_assert=>assert_equals( msg = '4th token of 2nd group' exp = |'B'| act = groups_of_tokens[ 2 ]->get_token( 4 )->value ).

  ENDMETHOD.

ENDCLASS.
