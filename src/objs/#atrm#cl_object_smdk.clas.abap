CLASS /atrm/cl_object_smdk DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_smdk IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA: lv_name TYPE sobj_name, lv_where TYPE string.
    lv_name = me->key-obj_name.
    REPLACE ALL OCCURRENCES OF '''' IN lv_name WITH ''''''.

    CONCATENATE 'TYPE = ''X'' AND ENTITY = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'SDOKME' where_clause = lv_where object_field = 'MODELSPACE' object_type = 'SMDM' CHANGING dependencies = dependencies.

    CONCATENATE 'CX_CLASS = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies EXPORTING table_name = 'SDOKMXA' where_clause = lv_where object_field = 'ATTRIBUTE' object_type = 'SMDA' CHANGING dependencies = dependencies.
  ENDMETHOD.

ENDCLASS.
