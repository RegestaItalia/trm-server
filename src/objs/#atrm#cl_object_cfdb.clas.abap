CLASS /atrm/cl_object_cfdb DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_cfdb IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      lv_name  TYPE sobj_name,
      lv_where TYPE string.

    lv_name = me->key-obj_name.
    REPLACE ALL OCCURRENCES OF '''' IN lv_name WITH ''''''.

    CONCATENATE 'BUSINESS_CONTEXT = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_BUS_CTXT'
        where_clause = lv_where
        object_field = 'VDM_CORE_VIEW'
        object_type  = 'CDS'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'BUSINESS_CONTEXT = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_BUS_CTXT'
        where_clause = lv_where
        object_field = 'DRAFT_VIEW_INCLUDE'
        object_type  = 'TABL'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'BUSINESS_CONTEXT = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_BUS_CTXT'
        where_clause = lv_where
        object_field = 'VIEW_INCLUDE'
        object_type  = 'TABL'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'BUSINESS_CONTEXT = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_BUS_CTXT'
        where_clause = lv_where
        object_field = 'CHANGE_INDICATOR_INCLUDE'
        object_type  = 'TABL'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'BUSINESS_CONTEXT = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_BUS_CTXT'
        where_clause = lv_where
        object_field = 'FIELD_CONTROL_INCLUDE'
        object_type  = 'TABL'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'BUSINESS_CONTEXT = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_BUS_CTXT'
        where_clause = lv_where
        object_field = 'PERSISTENCE_INCLUDE'
        object_type  = 'TABL'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'BUSINESS_CONTEXT = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_BUS_CTXT'
        where_clause = lv_where
        object_field = 'TRANSIENT_FIELDS_INCLUDE'
        object_type  = 'TABL'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'BUSINESS_CONTEXT = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_BUS_CTXT'
        where_clause = lv_where
        object_field = 'MESSAGE_CLASS'
        object_type  = 'MSAG'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'BUSINESS_CONTEXT = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_BUS_SUC'
        where_clause = lv_where
        object_field = 'SUCCESSOR'
        object_type  = 'CFDB'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'BUSINESS_CONTEXT = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_BUS_CT_IRF'
        where_clause = lv_where
        object_field = 'FM_MAPPING_CALLBACK'
        object_type  = 'FUNC'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'BUSINESS_CONTEXT = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_BUS_CT_IRF'
        where_clause = lv_where
        object_field = 'CHILD_TABLE'
        object_type  = 'TABL'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'BUSINESS_CONTEXT = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_BUS_CT_IRF'
        where_clause = lv_where
        object_field = 'PARENT_TABLE'
        object_type  = 'TABL'
      CHANGING
        dependencies = dependencies.
  ENDMETHOD.

ENDCLASS.
