CLASS /atrm/cl_object_suso DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS /atrm/cl_object_suso IMPLEMENTATION.
  METHOD /atrm/if_object~get_dependencies.
    DATA: lv_name  TYPE string,
          lv_where TYPE string.

    super->/atrm/if_object~get_dependencies(
      IMPORTING dependencies = dependencies
    ).

    lv_name = me->key-obj_name.
    REPLACE ALL OCCURRENCES OF `'` IN lv_name WITH `''`.

    " Object class
    CONCATENATE `OBJCT = '` lv_name `'` INTO lv_where.
    append_table_dependencies(
      EXPORTING table_name   = 'TOBJ'
                where_clause = lv_where
                object_field = 'OCLSS'
                object_type  = 'SUSC'
      CHANGING  dependencies = dependencies ).

    " Search helps of the object fields
    CONCATENATE `OBJECT = '` lv_name `'` INTO lv_where.
    append_table_dependencies(
      EXPORTING table_name   = 'TOBJ_FLD_EXT'
                where_clause = lv_where
                object_field = 'F4SEARCHHELP'
                object_type  = 'SHLP'
      CHANGING  dependencies = dependencies ).

    SORT dependencies BY tabname tabkey.
    DELETE ADJACENT DUPLICATES FROM dependencies COMPARING tabname tabkey.
  ENDMETHOD.
ENDCLASS.
