CLASS /atrm/cl_object_sito DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_sito IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA: lv_name TYPE sobj_name, lv_where TYPE string.
    lv_name = me->key-obj_name.
    REPLACE ALL OCCURRENCES OF '''' IN lv_name WITH ''''''.
    CONCATENATE 'SITNOBJECTID = ''' lv_name '''' INTO lv_where.

    CALL METHOD append_table_dependencies EXPORTING table_name = 'SIT2_OBJ_STRUC' where_clause = lv_where object_field = 'SITNOBJSTRUCID' object_type = 'CDS' CHANGING dependencies = dependencies.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'SIT2_OBJ_CB' where_clause = lv_where object_field = 'SITNCALLBACKCLASSNAME' object_type = 'CLAS' CHANGING dependencies = dependencies.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'SIT2_OBJ_VH_S' where_clause = lv_where object_field = 'SITNOBJVHSRVCBINDING' object_type = 'SRVB' CHANGING dependencies = dependencies.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'SIT2_OBJ_VH_S' where_clause = lv_where object_field = 'SITNOBJVHSRVCDEFINITION' object_type = 'SRVD' CHANGING dependencies = dependencies.

    CONCATENATE 'SITNOBJECTID = ''' lv_name '''' INTO lv_where.
    CONCATENATE lv_where ' AND SITNOBJEVENTCATEGORY = ''CL''' INTO lv_where.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'SIT2_OBJ_EVENTS' where_clause = lv_where object_field = 'SITNOBJEVENTTYPE' object_type = 'CLAS' CHANGING dependencies = dependencies.

    CONCATENATE 'SITNOBJECTID = ''' lv_name '''' INTO lv_where.
    CONCATENATE lv_where ' AND SITNOBJEVENTCATEGORY = ''BO''' INTO lv_where.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'SIT2_OBJ_EVENTS' where_clause = lv_where object_field = 'SITNOBJEVENTTYPE' object_type = 'SOBJ' CHANGING dependencies = dependencies.
  ENDMETHOD.

ENDCLASS.
