CLASS /atrm/cl_object_smd4 DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_smd4 IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA: lv_name TYPE sobj_name, lv_where TYPE string.
    lv_name = me->key-obj_name.
    REPLACE ALL OCCURRENCES OF '''' IN lv_name WITH ''''''.
    CONCATENATE 'TYPE = ''4'' AND ENTITY = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'SDOKME' where_clause = lv_where object_field = 'MODELSPACE' object_type = 'SMDM' CHANGING dependencies = dependencies.

    CONCATENATE 'IO_CLASS = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'SDOKMIA' where_clause = lv_where object_field = 'ATTRIBUTE' object_type = 'SMDA' CHANGING dependencies = dependencies.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'SDOKMITAB' where_clause = lv_where object_field = 'HEADERTAB' object_type = 'TABL' CHANGING dependencies = dependencies.

    CONCATENATE 'CLASS = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_sdok_class_deps EXPORTING table_name = 'SDOKMCLCL' where_clause = lv_where class_field = 'V_CLASS' CHANGING dependencies = dependencies.
  ENDMETHOD.

ENDCLASS.
