*"* use this source file for your ABAP unit test classes

CLASS ltc_main DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS create FOR TESTING.
ENDCLASS.

CLASS ltc_main IMPLEMENTATION.

  METHOD create.

    DATA(scabsrc) = NEW zcl_scabsrc( it_srcline = VALUE #(
        ( |  REPORT  .| )
        ( |  FORM test.| )
        ( |  ENDFORM.| ) ) ).
    DATA(blocks) = zcl_scabsrc_blocks=>create( scabsrc = scabsrc stmnt_type = scan_struc_stmnt_type-form ).
    cl_abap_unit_assert=>assert_equals( msg = 'number of blocks' exp = 1 act = blocks->count ).
    DATA(list_of_subroutines) = VALUE string_table(
        FOR i = 1 THEN i + 1 WHILE i <= blocks->count
        ( blocks->get_next( )->get_first_statement( )->get_token( 2 )->value ) ).
    cl_abap_unit_assert=>assert_equals( msg = 'list of subroutines' exp = value string_table( ( |TEST| ) ) act = list_of_subroutines ).

  ENDMETHOD.

ENDCLASS.
