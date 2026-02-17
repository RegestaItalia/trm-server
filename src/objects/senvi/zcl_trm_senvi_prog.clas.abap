CLASS zcl_trm_senvi_prog DEFINITION
  PUBLIC
  INHERITING FROM zcl_trm_senvi_map
  CREATE PUBLIC.

  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS determine REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_trm_senvi_prog IMPLEMENTATION.

  METHOD determine.
    DATA: obj_name TYPE string,
          regex    TYPE REF TO cl_abap_regex,
          matcher  TYPE REF TO cl_abap_matcher.
    obj_name = senvi-object.
    CREATE OBJECT regex
      EXPORTING
        pattern     = '^(\/[^\/]+\/)?SAPL'
        ignore_case = 'X'.
    matcher = regex->create_matcher( text = obj_name ).
    " if matches it's an include
    IF matcher->match( ) EQ 'X'.
      REPLACE FIRST OCCURRENCE OF REGEX '^(\/[^\/]+\/)?SAPL' IN obj_name WITH '' IGNORING CASE.
      APPEND zcl_trm_object=>get_tadir_dependency(
        object   = 'FUGR'
        obj_name = sanitize_object_name( raw_name = obj_name max_length = 26 )
      ) TO dependencies.
    ELSE.
      APPEND zcl_trm_object=>get_tadir_dependency(
        object   = 'PROG'
        obj_name = sanitize_object_name( raw_name = obj_name max_length = 30 )
      ) TO dependencies.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
