CLASS /atrm/cl_object_scp2 DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_scp2 IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA: lv_name TYPE sobj_name, lv_where TYPE string.
    lv_name = me->key-obj_name.
    REPLACE ALL OCCURRENCES OF '''' IN lv_name WITH ''''''.
    CONCATENATE 'ID = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'SCPRSATTR' where_clause = lv_where object_field = 'REFNAME' object_type = 'SCP2' CHANGING dependencies = dependencies.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'SCPRSVALS' where_clause = lv_where object_field = 'TABLENAME' object_type = 'TABL' CHANGING dependencies = dependencies.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'SCPRSKEYS' where_clause = lv_where object_field = 'TABLENAME' object_type = 'TABL' CHANGING dependencies = dependencies.
    CALL METHOD append_typed_dependencies EXPORTING table_name = 'SCPRSRECA' where_clause = lv_where object_type_field = 'OBJECTTYPE' object_field = 'OBJECTNAME' CHANGING dependencies = dependencies.
  ENDMETHOD.

ENDCLASS.
