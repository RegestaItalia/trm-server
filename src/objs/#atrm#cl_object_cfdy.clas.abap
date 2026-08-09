CLASS /atrm/cl_object_cfdy DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_cfdy IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      lv_name  TYPE sobj_name,
      lv_where TYPE string.

    lv_name = me->key-obj_name.
    REPLACE ALL OCCURRENCES OF '''' IN lv_name WITH ''''''.

    CONCATENATE 'ABSTRACT_ENTITY_NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_ABS'
        where_clause = lv_where
        object_field = 'ABSTRACT_ENTITY_NAME'
        object_type  = 'CDS'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'ABSTRACT_ENTITY_NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_ABS_CTXT'
        where_clause = lv_where
        object_field = 'BUSINESS_CONTEXT'
        object_type  = 'CFDB'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'ABSTRACT_ENTITY_NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_ABS_OP'
        where_clause = lv_where
        object_field = 'BDEF'
        object_type  = 'BDEF'
      CHANGING
        dependencies = dependencies.
  ENDMETHOD.

ENDCLASS.
