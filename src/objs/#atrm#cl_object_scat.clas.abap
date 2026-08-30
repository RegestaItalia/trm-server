CLASS /atrm/cl_object_scat DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_scat IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      lv_name   TYPE sobj_name,
      lv_where  TYPE string.

    lv_name = me->key-obj_name.
    REPLACE ALL OCCURRENCES OF '''' IN lv_name WITH ''''''.
    CONCATENATE 'TC_NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'TNODE02_A' where_clause = lv_where object_field = 'CLASS' object_type = 'CLAS' CHANGING dependencies = dependencies.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'CATF' where_clause = lv_where object_field = 'MPOOL' object_type = 'PROG' CHANGING dependencies = dependencies.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'CATF' where_clause = lv_where object_field = 'TCODE' object_type = 'TRAN' CHANGING dependencies = dependencies.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'CATP' where_clause = lv_where object_field = 'PDTEL' object_type = 'DTEL' CHANGING dependencies = dependencies.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'CATP' where_clause = lv_where object_field = 'PDOMA' object_type = 'DOMA' CHANGING dependencies = dependencies.
  ENDMETHOD.

ENDCLASS.
