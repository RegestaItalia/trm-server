CLASS /atrm/cl_object_scbo DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_scbo IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA: lv_name TYPE sobj_name, lv_where TYPE string.
    lv_name = me->key-obj_name.
    REPLACE ALL OCCURRENCES OF '''' IN lv_name WITH ''''''.
    CONCATENATE 'BUSINESS_OBJECT_ID = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'SCBO_OBJECT' where_clause = lv_where object_field = 'BSP_APP_NAME' object_type = 'WAPA' CHANGING dependencies = dependencies.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'SCBO_OBJECT' where_clause = lv_where object_field = 'IWSV_NAME' object_type = 'IWSV' CHANGING dependencies = dependencies.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'SCBO_OBJECT' where_clause = lv_where object_field = 'IWSG_NAME' object_type = 'IWSG' CHANGING dependencies = dependencies.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'SCBO_OBJECT' where_clause = lv_where object_field = 'CHANGE_DOCUMENT_OBJECT' object_type = 'CHDO' CHANGING dependencies = dependencies.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'SCBO_OBJECT' where_clause = lv_where object_field = 'ILM_OBJECT' object_type = 'ILMB' CHANGING dependencies = dependencies.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'SCBO_OBJECT' where_clause = lv_where object_field = 'DESTRUCTION_OBJECT' object_type = 'DOBJ' CHANGING dependencies = dependencies.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'SCBO_OBJECT' where_clause = lv_where object_field = 'DESTRUCTION_PROGRAM' object_type = 'PROG' CHANGING dependencies = dependencies.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'SCBO_NODE' where_clause = lv_where object_field = 'TABLE_NAME' object_type = 'TABL' CHANGING dependencies = dependencies.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'SCBO_NODE' where_clause = lv_where object_field = 'ALTERNATIVEKEY_TABLE_NAME' object_type = 'TABL' CHANGING dependencies = dependencies.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'SCBO_NODE' where_clause = lv_where object_field = 'CDS_VIEW_NAME' object_type = 'CDS' CHANGING dependencies = dependencies.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'SCBO_NODE' where_clause = lv_where object_field = 'VH_CDS_VIEW_NAME' object_type = 'CDS' CHANGING dependencies = dependencies.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'SCBO_NODE' where_clause = lv_where object_field = 'UI_METADATA_EXTENSION_NAME' object_type = 'DDLX' CHANGING dependencies = dependencies.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'SCBO_NODE' where_clause = lv_where object_field = 'BL_EVAL_RS_CLASS_NAME' object_type = 'CLAS' CHANGING dependencies = dependencies.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'SCBO_NODE' where_clause = lv_where object_field = 'BL_EVAL_RS_INTF_NAME' object_type = 'INTF' CHANGING dependencies = dependencies.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'SCBO_NODE' where_clause = lv_where object_field = 'DCL_NAME' object_type = 'DCLS' CHANGING dependencies = dependencies.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'SCBO_ELEMENT' where_clause = lv_where object_field = 'CUSTOM_VH_VIEW' object_type = 'CDS' CHANGING dependencies = dependencies.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'SCBO_ELEMENT' where_clause = lv_where object_field = 'ASSOC_BUSINESS_OBJECT_ID' object_type = 'SCBO' CHANGING dependencies = dependencies.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'SCBO_BL_EVAL' where_clause = lv_where object_field = 'CLASS_NAME_BOPF' object_type = 'CLAS' CHANGING dependencies = dependencies.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'SCBO_BL_EVAL' where_clause = lv_where object_field = 'CLASS_NAME_KEY_USER' object_type = 'CLAS' CHANGING dependencies = dependencies.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'SCBO_BL_ACTION' where_clause = lv_where object_field = 'PARAMETER_STRUCTURE_NAME' object_type = 'TABL' CHANGING dependencies = dependencies.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'SCBO_BL_ACTION' where_clause = lv_where object_field = 'CLASS_NAME_BOPF' object_type = 'CLAS' CHANGING dependencies = dependencies.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'SCBO_BL_ACTION' where_clause = lv_where object_field = 'CLASS_NAME_KEY_USER' object_type = 'CLAS' CHANGING dependencies = dependencies.
  ENDMETHOD.

ENDCLASS.
