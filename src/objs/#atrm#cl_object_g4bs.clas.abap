CLASS /atrm/cl_object_g4bs DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_g4bs IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      lv_name    TYPE sobj_name,
      lv_id      TYPE sobj_name,
      lv_version TYPE sobj_name,
      lv_where   TYPE string.

    lv_name = me->key-obj_name.
    lv_id = lv_name(36).
    lv_version = lv_name+36(4).
    REPLACE ALL OCCURRENCES OF '''' IN lv_id WITH ''''''.
    REPLACE ALL OCCURRENCES OF '''' IN lv_version WITH ''''''.
    CONCATENATE 'SERVICE_ID = ''' lv_id
      ''' AND SERVICE_VERSION = ''' lv_version '''' INTO lv_where.

    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = '/IWBEP/I_V4_MSRV'
        where_clause = lv_where
        object_field = 'MPC_NAME'
        object_type  = 'CLAS'
      CHANGING
        dependencies = dependencies.

    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = '/IWBEP/I_V4_MSRV'
        where_clause = lv_where
        object_field = 'DPC_NAME'
        object_type  = 'CLAS'
      CHANGING
        dependencies = dependencies.
  ENDMETHOD.

ENDCLASS.
