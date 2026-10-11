CLASS /atrm/cl_object_cfdp DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_cfdp IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      lv_name  TYPE sobj_name,
      lv_where TYPE string.

    lv_name = me->key-obj_name.
    REPLACE ALL OCCURRENCES OF '''' IN lv_name WITH ''''''.

    CONCATENATE 'SERVICE_INTERFACE_NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_SOAP'
        where_clause = lv_where
        object_field = 'SERVICE_INTERFACE_NAME'
        object_type  = 'SPRX'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'SERVICE_INTERFACE_NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_SOAP_CTXT'
        where_clause = lv_where
        object_field = 'BUSINESS_CONTEXT'
        object_type  = 'CFDB'
      CHANGING
        dependencies = dependencies.
  ENDMETHOD.

ENDCLASS.
