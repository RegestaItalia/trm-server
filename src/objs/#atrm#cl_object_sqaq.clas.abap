CLASS /atrm/cl_object_sqaq DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_sqaq IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA: lv_name TYPE sobj_name, lv_where TYPE string.
    lv_name = me->key-obj_name.
    REPLACE ALL OCCURRENCES OF '''' IN lv_name WITH ''''''.

    CONCATENATE 'QUEID = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'SQADB01' where_clause = lv_where object_field = 'CHTAB' object_type = 'TABL' CHANGING dependencies = dependencies.
    CALL METHOD append_typed_dependencies EXPORTING table_name = 'SQADB03CH' where_clause = lv_where object_type_field = 'OBJTYPE' object_field = 'OBJID' CHANGING dependencies = dependencies.
  ENDMETHOD.

ENDCLASS.
