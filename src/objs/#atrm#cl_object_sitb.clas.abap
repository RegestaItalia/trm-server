CLASS /atrm/cl_object_sitb DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_sitb IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA: lv_name TYPE sobj_name, lv_where TYPE string.
    lv_name = me->key-obj_name.
    REPLACE ALL OCCURRENCES OF '''' IN lv_name WITH ''''''.
    CONCATENATE 'SITNBASETEMPLATEID = ''' lv_name '''' INTO lv_where.

    CALL METHOD append_table_dependencies EXPORTING table_name = 'SIT2_BT_OBJ_GRP' where_clause = lv_where object_field = 'SITNANCHOROBJECTID' object_type = 'SITO' CHANGING dependencies = dependencies.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'SIT2_BT_OBJ_GRP' where_clause = lv_where object_field = 'SITNTRIGGEROBJECTID' object_type = 'SITO' CHANGING dependencies = dependencies.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'SIT2_BT_AO' where_clause = lv_where object_field = 'SITNANCHOROBJECTID' object_type = 'SITO' CHANGING dependencies = dependencies.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'SIT2_BT_ACT' where_clause = lv_where object_field = 'SITNOBJECTID' object_type = 'SITO' CHANGING dependencies = dependencies.
  ENDMETHOD.

ENDCLASS.
