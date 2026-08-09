CLASS /atrm/cl_object_edom DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_edom IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA: lv_name TYPE sobj_name, lv_where TYPE string.
    lv_name = me->key-obj_name.
    REPLACE ALL OCCURRENCES OF '''' IN lv_name WITH ''''''.

    CONCATENATE 'MESSAGE_TYPE = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'EDOMSGTYPEINT' where_clause = lv_where object_field = 'INTERFACE_ID_OUT' object_type = 'EDOI' CHANGING dependencies = dependencies.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'EDOMSGTYPEINT' where_clause = lv_where object_field = 'INTERFACE_ID_IN' object_type = 'EDOI' CHANGING dependencies = dependencies.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'EDOMSGTYPEINT' where_clause = lv_where object_field = 'MAPPING_CLASS' object_type = 'CLAS' CHANGING dependencies = dependencies.

    CONCATENATE 'PULLING_MSGTYPE = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'EDOMSGTYPEPULL' where_clause = lv_where object_field = 'PREP_MSGTYPE' object_type = 'EDOM' CHANGING dependencies = dependencies.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'EDOMSGTYPEPULL' where_clause = lv_where object_field = 'CLEANUP_MSGTYPE' object_type = 'EDOM' CHANGING dependencies = dependencies.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'EDOMSGTYPEPULL' where_clause = lv_where object_field = 'HANDLING_CLASS' object_type = 'CLAS' CHANGING dependencies = dependencies.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'EDOMSGTYPEPULL' where_clause = lv_where object_field = 'EDOC_TYPE' object_type = 'EDOT' CHANGING dependencies = dependencies.

    CONCATENATE 'PUSHING_MSGTYPE = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'EDOMSGTYPEPUSH' where_clause = lv_where object_field = 'HANDLING_CLASS' object_type = 'CLAS' CHANGING dependencies = dependencies.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'EDOMSGTYPEPUSH' where_clause = lv_where object_field = 'EDOC_TYPE' object_type = 'EDOT' CHANGING dependencies = dependencies.

    CONCATENATE 'MSGTYPE_UPLOAD = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'EDOMSGTYPEUPLD' where_clause = lv_where object_field = 'HANDLING_CLASS' object_type = 'CLAS' CHANGING dependencies = dependencies.
  ENDMETHOD.

ENDCLASS.
