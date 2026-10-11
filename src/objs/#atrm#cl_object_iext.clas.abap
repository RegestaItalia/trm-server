CLASS /atrm/cl_object_iext DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_iext IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      lv_name  TYPE string,
      lv_where TYPE string.

    lv_name = me->key-obj_name.
    REPLACE ALL OCCURRENCES OF `'` IN lv_name WITH `''`.
    CONCATENATE `CIMTYP = '` lv_name `'` INTO lv_where.

    " Extended basic type
    append_table_dependencies(
      EXPORTING table_name   = 'EDCIM'
                where_clause = lv_where
                object_field = 'IDOCTYP'
                object_type  = 'IDOC'
      CHANGING  dependencies = dependencies ).

    " Predecessor extension
    append_table_dependencies(
      EXPORTING table_name   = 'EDCIM'
                where_clause = lv_where
                object_field = 'PRETYP'
                object_type  = 'IEXT'
      CHANGING  dependencies = dependencies ).

    " Extension segment types
    append_table_dependencies(
      EXPORTING table_name   = 'CIMSYN'
                where_clause = lv_where
                object_field = 'CIMSTP'
                object_type  = 'TABL'
      CHANGING  dependencies = dependencies ).

    SORT dependencies BY tabname tabkey.
    DELETE ADJACENT DUPLICATES FROM dependencies COMPARING tabname tabkey.
  ENDMETHOD.

ENDCLASS.
