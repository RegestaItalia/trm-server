CLASS /atrm/cl_object_dial DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS /atrm/cl_object_dial IMPLEMENTATION.
  METHOD /atrm/if_object~get_dependencies.
    DATA: lv_name  TYPE string,
          lv_where TYPE string.

    super->/atrm/if_object~get_dependencies(
      IMPORTING dependencies = dependencies
    ).

    lv_name = me->key-obj_name.
    REPLACE ALL OCCURRENCES OF `'` IN lv_name WITH `''`.

    " Module pool program of the dialog module
    CONCATENATE `DNAM = '` lv_name `'` INTO lv_where.
    append_table_dependencies(
      EXPORTING table_name   = 'TDCT'
                where_clause = lv_where
                object_field = 'PROG'
                object_type  = 'PROG'
      CHANGING  dependencies = dependencies ).

    SORT dependencies BY tabname tabkey.
    DELETE ADJACENT DUPLICATES FROM dependencies COMPARING tabname tabkey.
  ENDMETHOD.
ENDCLASS.
