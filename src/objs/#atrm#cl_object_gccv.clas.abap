CLASS /atrm/cl_object_gccv DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_gccv IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      lv_name  TYPE sobj_name,
      lv_where TYPE string.

    lv_name = me->key-obj_name.
    REPLACE ALL OCCURRENCES OF '''' IN lv_name WITH ''''''.
    CONCATENATE 'CHECK_IDENTIFIER = ''' lv_name '''' INTO lv_where.

    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'SCMI_CCV_CLASS'
        where_clause = lv_where
        object_field = 'CHECK_NAME'
        object_type  = 'CLAS'
      CHANGING
        dependencies = dependencies.

    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'SCMI_CCV_CLASS'
        where_clause = lv_where
        object_field = 'FEATURE_TOGGLE_ID'
        object_type  = 'FTGL'
      CHANGING
        dependencies = dependencies.
  ENDMETHOD.

ENDCLASS.
