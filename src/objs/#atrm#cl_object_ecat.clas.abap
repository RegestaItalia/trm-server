CLASS /atrm/cl_object_ecat DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_ecat IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      lv_name  TYPE sobj_name,
      lv_where TYPE string.

    lv_name = me->key-obj_name.
    REPLACE ALL OCCURRENCES OF '''' IN lv_name WITH ''''''.

    CONCATENATE 'NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'ECATT_VER'
        where_clause = lv_where
        object_field = 'SYSTEMDATA'
        object_type  = 'ECSD'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_senvi_table_deps
      EXPORTING
        table_name   = 'ECOBJUSE'
        where_clause = lv_where
        type_field   = 'WBOBJ_TYPE'
        object_field = 'WBOBJ_COMP1'
        origin       = me
      CHANGING
        dependencies = dependencies.
  ENDMETHOD.

ENDCLASS.
