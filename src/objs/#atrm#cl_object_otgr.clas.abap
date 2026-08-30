CLASS /atrm/cl_object_otgr DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_otgr IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      lv_name   TYPE sobj_name,
      lv_where  TYPE string.

    lv_name = me->key-obj_name.
    REPLACE ALL OCCURRENCES OF '''' IN lv_name WITH ''''''.
    CONCATENATE 'OBJ_TYPE_GROUP = ''' lv_name '''' INTO lv_where.

    CALL METHOD append_table_dependencies
      EXPORTING
        table_name    = 'CLS_TYGR_PARENT'
        where_clause  = lv_where
        object_field  = 'PARENT'
        object_type   = 'OTGR'
      CHANGING
        dependencies  = dependencies.
  ENDMETHOD.

ENDCLASS.
