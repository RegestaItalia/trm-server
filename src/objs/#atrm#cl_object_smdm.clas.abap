CLASS /atrm/cl_object_smdm DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_smdm IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA: lv_name TYPE sobj_name, lv_where TYPE string.
    lv_name = me->key-obj_name.
    REPLACE ALL OCCURRENCES OF '''' IN lv_name WITH ''''''.

    CONCATENATE 'MODELSPACE = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_sdok_class_deps EXPORTING table_name = 'SDOKME' where_clause = lv_where class_field = 'ENTITY' CHANGING dependencies = dependencies.
  ENDMETHOD.

ENDCLASS.
