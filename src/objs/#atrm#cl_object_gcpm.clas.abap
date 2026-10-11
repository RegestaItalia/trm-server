CLASS /atrm/cl_object_gcpm DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_gcpm IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      lv_model   TYPE sobj_name,
      lv_version TYPE sobj_name,
      lv_where   TYPE string.

    lv_model = me->key-obj_name(36).
    lv_version = me->key-obj_name+36(4).
    REPLACE ALL OCCURRENCES OF '''' IN lv_model WITH ''''''.
    REPLACE ALL OCCURRENCES OF '''' IN lv_version WITH ''''''.
    CONCATENATE 'PROXY_MODEL_ID = ''' lv_model
      ''' AND VERSION = ''' lv_version '''' INTO lv_where.

    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = '/IWBEP/I_CP_MOD'
        where_clause = lv_where
        object_field = 'MPC_NAME'
        object_type  = 'CLAS'
      CHANGING
        dependencies = dependencies.
  ENDMETHOD.

ENDCLASS.
