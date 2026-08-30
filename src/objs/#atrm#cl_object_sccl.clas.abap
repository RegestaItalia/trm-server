CLASS /atrm/cl_object_sccl DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_sccl IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA: lv_name TYPE sobj_name, lv_where TYPE string.
    lv_name = me->key-obj_name.
    REPLACE ALL OCCURRENCES OF '''' IN lv_name WITH ''''''.
    CONCATENATE 'CODELISTID = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'SCCL_LIST' where_clause = lv_where object_field = 'CDS_VIEW_NAME_C' object_type = 'CDS' CHANGING dependencies = dependencies.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'SCCL_LIST' where_clause = lv_where object_field = 'CDS_VIEW_NAME_T' object_type = 'CDS' CHANGING dependencies = dependencies.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'SCCL_LIST' where_clause = lv_where object_field = 'DATA_ELEMENT_NAME_C' object_type = 'DTEL' CHANGING dependencies = dependencies.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'SCCL_LIST' where_clause = lv_where object_field = 'DATA_ELEMENT_NAME_T' object_type = 'DTEL' CHANGING dependencies = dependencies.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'SCCL_LIST' where_clause = lv_where object_field = 'DOMAIN_NAME_C' object_type = 'DOMA' CHANGING dependencies = dependencies.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'SCCL_LIST' where_clause = lv_where object_field = 'DOMAIN_NAME_T' object_type = 'DOMA' CHANGING dependencies = dependencies.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'SCCL_LIST' where_clause = lv_where object_field = 'TABLE_NAME_C' object_type = 'TABL' CHANGING dependencies = dependencies.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'SCCL_LIST' where_clause = lv_where object_field = 'TABLE_NAME_T' object_type = 'TABL' CHANGING dependencies = dependencies.
  ENDMETHOD.

ENDCLASS.
