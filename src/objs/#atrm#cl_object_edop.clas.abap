CLASS /atrm/cl_object_edop DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_edop IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA: lv_name TYPE sobj_name, lv_where TYPE string.
    lv_name = me->key-obj_name.
    REPLACE ALL OCCURRENCES OF '''' IN lv_name WITH ''''''.
    CONCATENATE 'PROCESS = ''' lv_name '''' INTO lv_where.

    CALL METHOD append_table_dependencies EXPORTING table_name = 'EDOPROCESS' where_clause = lv_where object_field = 'REF_PROCESS' object_type = 'EDOP' CHANGING dependencies = dependencies.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'EDOPROCSTEP' where_clause = lv_where object_field = 'CLASS' object_type = 'CLAS' CHANGING dependencies = dependencies.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'EDOPROCSTEPDET' where_clause = lv_where object_field = 'EDOC_TYPE' object_type = 'EDOT' CHANGING dependencies = dependencies.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'EDOPROCSTEPDET' where_clause = lv_where object_field = 'INTERFACE_ID' object_type = 'EDOI' CHANGING dependencies = dependencies.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'EDOPROCSPINTDET' where_clause = lv_where object_field = 'EDOC_TYPE' object_type = 'EDOT' CHANGING dependencies = dependencies.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'EDOPROCSPINTDET' where_clause = lv_where object_field = 'INTERFACE_ID' object_type = 'EDOI' CHANGING dependencies = dependencies.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'EDOPROCSPINTDET' where_clause = lv_where object_field = 'MAPPING_CLASS' object_type = 'CLAS' CHANGING dependencies = dependencies.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'EDOMAPCLASSDET' where_clause = lv_where object_field = 'EDOC_TYPE' object_type = 'EDOT' CHANGING dependencies = dependencies.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'EDOMAPCLASSDET' where_clause = lv_where object_field = 'INTERFACE_ID' object_type = 'EDOI' CHANGING dependencies = dependencies.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'EDOMAPCLASSDET' where_clause = lv_where object_field = 'MAPPING_CLASS' object_type = 'CLAS' CHANGING dependencies = dependencies.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'EDOACTIONUIPROC' where_clause = lv_where object_field = 'UI_CLASS' object_type = 'CLAS' CHANGING dependencies = dependencies.
  ENDMETHOD.

ENDCLASS.
