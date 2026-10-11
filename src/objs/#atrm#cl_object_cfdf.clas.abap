CLASS /atrm/cl_object_cfdf DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_cfdf IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      lv_name  TYPE sobj_name,
      lv_where TYPE string.

    lv_name = me->key-obj_name.
    REPLACE ALL OCCURRENCES OF '''' IN lv_name WITH ''''''.

    CONCATENATE 'NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_REP'
        where_clause = lv_where
        object_field = 'DATA_ELEMENT_NAME'
        object_type  = 'DTEL'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_REP'
        where_clause = lv_where
        object_field = 'LEADING_DIMENSION_VIEW_NAME'
        object_type  = 'CDS'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_REP'
        where_clause = lv_where
        object_field = 'VALUE_HELP_VIEW_NAME'
        object_type  = 'CDS'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_REP'
        where_clause = lv_where
        object_field = 'ASSOCIATED_BUSINESS_OBJECT'
        object_type  = 'CFDR'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_REP'
        where_clause = lv_where
        object_field = 'BADI_IMPL_FOR_DATA_SUBJECT'
        object_type  = 'SXCI'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_REP_BUS'
        where_clause = lv_where
        object_field = 'BUSINESS_CONTEXT'
        object_type  = 'CFDB'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_REP_BUS'
        where_clause = lv_where
        object_field = 'DRAFT_VIEW_INCLUDE'
        object_type  = 'TABL'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_REP_BUS'
        where_clause = lv_where
        object_field = 'VIEW_INCLUDE'
        object_type  = 'TABL'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_REP_BUS'
        where_clause = lv_where
        object_field = 'CHANGE_INDICATOR_INCLUDE'
        object_type  = 'TABL'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_REP_BUS'
        where_clause = lv_where
        object_field = 'FIELD_CONTROL_INCLUDE'
        object_type  = 'TABL'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_REP_BUS'
        where_clause = lv_where
        object_field = 'PERSISTENCE_INCLUDE'
        object_type  = 'TABL'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_REP_BUS'
        where_clause = lv_where
        object_field = 'TRANSIENT_FIELDS_INCLUDE'
        object_type  = 'TABL'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_REP_CDS'
        where_clause = lv_where
        object_field = 'BUSINESS_CONTEXT'
        object_type  = 'CFDB'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_REP_CDS'
        where_clause = lv_where
        object_field = 'CDS_VIEW_NAME'
        object_type  = 'CFDC'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_REP_CDS'
        where_clause = lv_where
        object_field = 'EXTENSION_INCL_VIEW_NAME'
        object_type  = 'CDS'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_REP_CDS'
        where_clause = lv_where
        object_field = 'BADI_IMPLEMENTATION_FOR_HIDDEN'
        object_type  = 'SXCI'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_REP_ODATA'
        where_clause = lv_where
        object_field = 'BUSINESS_CONTEXT'
        object_type  = 'CFDB'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_REP_ODATA'
        where_clause = lv_where
        object_field = 'MODEL_NAME'
        object_type  = 'CFDO'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_REP_ODATA'
        where_clause = lv_where
        object_field = 'ROLE_FIELDS_INCLUDE'
        object_type  = 'TABL'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_REP_ODATA'
        where_clause = lv_where
        object_field = 'UNION_FIELDS_INCLUDE'
        object_type  = 'TABL'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_REP_SCN'
        where_clause = lv_where
        object_field = 'SCENARIO_NAME'
        object_type  = 'CFDS'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_REP_SCN_SP'
        where_clause = lv_where
        object_field = 'SCENARIO_NAME'
        object_type  = 'CFDS'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_REP_SCN_SP'
        where_clause = lv_where
        object_field = 'DATA_TRANSFER_NAME'
        object_type  = 'CFDT'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_REP_TRF'
        where_clause = lv_where
        object_field = 'DATA_TRANSFER_NAME'
        object_type  = 'CFDT'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_REP_GUI'
        where_clause = lv_where
        object_field = 'BUSINESS_CONTEXT'
        object_type  = 'CFDB'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_REP_GUI'
        where_clause = lv_where
        object_field = 'GUI_CONTEXT'
        object_type  = 'CFDG'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_REP_SOAP'
        where_clause = lv_where
        object_field = 'BUSINESS_CONTEXT'
        object_type  = 'CFDB'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_REP_SOAP'
        where_clause = lv_where
        object_field = 'SERVICE_INTERFACE_NAME'
        object_type  = 'CFDP'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_REP_VH_F'
        where_clause = lv_where
        object_field = 'DIMENSION_VIEW_NAME'
        object_type  = 'CDS'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_REP_VH_BDG'
        where_clause = lv_where
        object_field = 'CDS_VIEW_NAME'
        object_type  = 'CDS'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_REP_ABS'
        where_clause = lv_where
        object_field = 'BUSINESS_CONTEXT'
        object_type  = 'CFDB'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_REP_ABS'
        where_clause = lv_where
        object_field = 'ABSTRACT_ENTITY_NAME'
        object_type  = 'CFDY'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_typed_dependencies
      EXPORTING
        table_name        = 'CFD_W_REP_ENH'
        where_clause      = lv_where
        object_field      = 'ENHANCEMENT_OBJECT_NAME'
        object_type_field = 'ENHANCEMENT_OBJECT_TYPE'
      CHANGING
        dependencies      = dependencies.

    CONCATENATE 'NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_typed_dependencies
      EXPORTING
        table_name        = 'CFD_W_REP_ENH_FT'
        where_clause      = lv_where
        object_field      = 'CONTENT_OBJECT_NAME'
        object_type_field = 'ENHANCEMENT_OBJECT_TYPE'
      CHANGING
        dependencies      = dependencies.

    CONCATENATE 'NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_typed_dependencies
      EXPORTING
        table_name        = 'CFD_W_REP_CH_REF'
        where_clause      = lv_where
        object_field      = 'CHARACTERISTIC_NAME'
        object_type_field = 'OBJECT_TYPE'
      CHANGING
        dependencies      = dependencies.

    CONCATENATE 'NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_typed_dependencies
      EXPORTING
        table_name        = 'CFD_W_REP_VH_BDG'
        where_clause      = lv_where
        object_field      = 'EXTENSION_OBJECT_NAME'
        object_type_field = 'EXTENSION_OBJECT_TYPE'
      CHANGING
        dependencies      = dependencies.

    CONCATENATE 'NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_typed_dependencies
      EXPORTING
        table_name        = 'CFD_W_REP_VH_BDG'
        where_clause      = lv_where
        object_field      = 'VH_EXTENSION_OBJECT_NAME'
        object_type_field = 'VH_EXTENSION_OBJECT_TYPE'
      CHANGING
        dependencies      = dependencies.
  ENDMETHOD.

ENDCLASS.
