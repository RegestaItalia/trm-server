CLASS /atrm/cl_object_idoc DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_idoc IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      lv_name  TYPE string,
      lv_where TYPE string.

    lv_name = me->key-obj_name.
    REPLACE ALL OCCURRENCES OF `'` IN lv_name WITH `''`.
    CONCATENATE `IDOCTYP = '` lv_name `'` INTO lv_where.

    " Segment types of the basic type
    append_table_dependencies(
      EXPORTING table_name   = 'IDOCSYN'
                where_clause = lv_where
                object_field = 'SEGTYP'
                object_type  = 'TABL'
      CHANGING  dependencies = dependencies ).

    " Predecessor basic type
    append_table_dependencies(
      EXPORTING table_name   = 'EDBAS'
                where_clause = lv_where
                object_field = 'PRETYP'
                object_type  = 'IDOC'
      CHANGING  dependencies = dependencies ).

    SORT dependencies BY tabname tabkey.
    DELETE ADJACENT DUPLICATES FROM dependencies COMPARING tabname tabkey.
  ENDMETHOD.

ENDCLASS.
