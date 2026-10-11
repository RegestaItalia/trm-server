CLASS /atrm/cl_object_scrl DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_scrl IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA: lv_name TYPE sobj_name, lv_where TYPE string.
    lv_name = me->key-obj_name.
    REPLACE ALL OCCURRENCES OF '''' IN lv_name WITH ''''''.
    CONCATENATE 'LIBRARY_ID = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'SCRL_LIBRARY' where_clause = lv_where object_field = 'IMPLEMENTATION_CLASS' object_type = 'CLAS' CHANGING dependencies = dependencies.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'SCRL_TABLE' where_clause = lv_where object_field = 'STRUCTURE' object_type = 'TABL' CHANGING dependencies = dependencies.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'SCRL_TABLE' where_clause = lv_where object_field = 'DATA_SOURCE' object_type = 'ENTY' CHANGING dependencies = dependencies.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'SCRL_TABLE' where_clause = lv_where object_field = 'CODE_LIST' object_type = 'SCCL' CHANGING dependencies = dependencies.
  ENDMETHOD.

ENDCLASS.
