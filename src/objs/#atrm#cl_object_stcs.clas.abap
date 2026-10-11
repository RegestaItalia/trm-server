CLASS /atrm/cl_object_stcs DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_stcs IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA: lv_name TYPE sobj_name, lv_where TYPE string.
    lv_name = me->key-obj_name.
    REPLACE ALL OCCURRENCES OF '''' IN lv_name WITH ''''''.

    CONCATENATE 'SCENARIO_ID = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'STC_SCN_HDR' where_clause = lv_where object_field = 'COPY_FROM' object_type = 'STCS' CHANGING dependencies = dependencies.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'STC_SCN_HDR' where_clause = lv_where object_field = 'BASIC_SCEN_ID' object_type = 'STCS' CHANGING dependencies = dependencies.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'STC_SCN_HDR' where_clause = lv_where object_field = 'CONFIG_CLASS' object_type = 'CLAS' CHANGING dependencies = dependencies.
    CALL METHOD append_typed_dependencies EXPORTING table_name = 'STC_SCN_TASKS' where_clause = lv_where object_type_field = 'TASKTYPE' object_field = 'TASKNAME' CHANGING dependencies = dependencies.
  ENDMETHOD.

ENDCLASS.
