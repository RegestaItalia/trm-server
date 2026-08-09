CLASS /atrm/cl_object_sble DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_sble IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA: lv_name TYPE sobj_name, lv_where TYPE string.
    lv_name = me->key-obj_name.
    REPLACE ALL OCCURRENCES OF '''' IN lv_name WITH ''''''.
    CONCATENATE 'IMPLEMENTATION_NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING table_name = 'SBLE_W_ROOT' where_clause = lv_where
                object_field = 'BADI_NAME' object_type = 'ENHS'
      CHANGING dependencies = dependencies.
  ENDMETHOD.

ENDCLASS.
