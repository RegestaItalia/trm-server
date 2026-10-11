CLASS /atrm/cl_object_pdac DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_pdac IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      lv_name   TYPE sobj_name,
      lv_where  TYPE string.

    lv_name = me->key-obj_name.
    REPLACE ALL OCCURRENCES OF '''' IN lv_name WITH ''''''.
    CONCATENATE 'OTYPE = ''AC'' AND OBJID = ''' lv_name ''''
      INTO lv_where.

    CALL METHOD append_table_dependencies
      EXPORTING
        table_name    = 'HRS1203'
        where_clause  = lv_where
        object_field  = 'FNAME'
        object_type   = 'FUNC'
      CHANGING
        dependencies  = dependencies.
  ENDMETHOD.

ENDCLASS.
