CLASS /atrm/cl_object_edot DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_edot IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA: lv_name TYPE sobj_name, lv_where TYPE string.
    lv_name = me->key-obj_name.
    REPLACE ALL OCCURRENCES OF '''' IN lv_name WITH ''''''.
    CONCATENATE 'EDOC_TYPE = ''' lv_name '''' INTO lv_where.

    CALL METHOD append_table_dependencies EXPORTING table_name = 'EDOTYPE' where_clause = lv_where object_field = 'TABNAME' object_type = 'TABL' CHANGING dependencies = dependencies.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'EDOTYPETAB' where_clause = lv_where object_field = 'TABNAME' object_type = 'TABL' CHANGING dependencies = dependencies.
  ENDMETHOD.

ENDCLASS.
