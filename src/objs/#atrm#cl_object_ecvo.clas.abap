CLASS /atrm/cl_object_ecvo DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_ecvo IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      lv_name  TYPE sobj_name,
      lv_where TYPE string.

    lv_name = me->key-obj_name.
    REPLACE ALL OCCURRENCES OF '''' IN lv_name WITH ''''''.

    CONCATENATE 'NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'ECVO_VER'
        where_clause = lv_where
        object_field = 'IMPL_NAME'
        object_type  = 'ECAT'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'ECVO_BUS_MSG'
        where_clause = lv_where
        object_field = 'ARBGB'
        object_type  = 'MSAG'
      CHANGING
        dependencies = dependencies.
  ENDMETHOD.

ENDCLASS.
