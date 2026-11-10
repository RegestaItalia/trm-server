CLASS /atrm/cl_object_sush DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_sush IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      lv_name     TYPE usobkey-name,
      lv_type     TYPE usobkey-type,
      lv_len      TYPE i,
      lv_nescape  TYPE string,
      lv_where    TYPE string.

    " Object name is the USOBKEY name followed by the 2-character type
    DESCRIBE FIELD lv_name LENGTH lv_len IN CHARACTER MODE.
    lv_name = me->key-obj_name(lv_len).
    lv_type = me->key-obj_name+lv_len(2).
    CHECK lv_name IS NOT INITIAL.

    lv_nescape = lv_name.
    REPLACE ALL OCCURRENCES OF `'` IN lv_nescape WITH `''`.

    " Hashed application (e.g. external service): object behind the hash
    CONCATENATE `NAME = '` lv_nescape `' AND TYPE = '` lv_type `' AND PGMID = 'R3TR'` INTO lv_where.
    append_typed_dependencies(
      EXPORTING table_name        = 'USOBHASH'
                where_clause      = lv_where
                object_field      = 'OBJ_NAME'
                object_type_field = 'OBJECT'
      CHANGING  dependencies      = dependencies ).

    " Proposed authorization objects
    CONCATENATE `NAME = '` lv_nescape `' AND TYPE = '` lv_type `'` INTO lv_where.
    append_table_dependencies(
      EXPORTING table_name   = 'USOBX'
                where_clause = lv_where
                object_field = 'OBJECT'
                object_type  = 'SUSO'
      CHANGING  dependencies = dependencies ).

    " RFC function module application: function group
    IF lv_type = 'RF'.
      CONCATENATE `FUNCNAME = '` lv_nescape `'` INTO lv_where.
      append_table_dependencies(
        EXPORTING table_name   = 'V_FDIR'
                  where_clause = lv_where
                  object_field = 'AREA'
                  object_type  = 'FUGR'
        CHANGING  dependencies = dependencies ).
    ENDIF.

    SORT dependencies BY tabname tabkey.
    DELETE ADJACENT DUPLICATES FROM dependencies COMPARING tabname tabkey.
  ENDMETHOD.

ENDCLASS.
