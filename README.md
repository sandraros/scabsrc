# scabsrc
OO wrapper of [SCAN ABAP-SOURCE](https://help.sap.com/doc/abapdocu_753_index_htm/7.53/en-US/index.htm?file=abapscan.htm)

**VERSION ALPHA - NOT RELEASED YET**

Example to get all subroutines of program SFBE_EXAMPLE7 (search statements **`FORM subr`**) :
```
DATA(scabsrc) = NEW zcl_scabsrc( i_program = 'SFBE_EXAMPLE7' ).
DATA(blocks) = scabsrc->get_blocks( stmnt_type = scan_struc_stmnt_type-form ).
DATA(list_of_subroutines) = VALUE string_table(
    FOR i = 1 THEN i + 1 WHILE i <= blocks->count
    ( blocks->get_next( )->get_first_statement( )->get_token( 2 )->value ) ).
```
