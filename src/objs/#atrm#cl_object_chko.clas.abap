CLASS /atrm/cl_object_chko DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_chko IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      lv_name  TYPE string,
      lv_where TYPE string.

    lv_name = me->key-obj_name.
    REPLACE ALL OCCURRENCES OF `'` IN lv_name WITH `''`.

    CONCATENATE `NAME = '` lv_name `' AND VERSION = 'A'` INTO lv_where.

    " Check category
    append_table_dependencies(
      EXPORTING table_name   = 'CHKO_CONTENT'
                where_clause = lv_where
                object_field = 'CATEGORY'
                object_type  = 'CHKC'
      CHANGING  dependencies = dependencies ).

    " Implementing check class
    append_table_dependencies(
      EXPORTING table_name   = 'CHKO_CONTENT'
                where_clause = lv_where
                object_field = 'IMPLEMENTING_CLASS'
                object_type  = 'CLAS'
      CHANGING  dependencies = dependencies ).

    SORT dependencies BY tabname tabkey.
    DELETE ADJACENT DUPLICATES FROM dependencies COMPARING tabname tabkey.
  ENDMETHOD.

ENDCLASS.
