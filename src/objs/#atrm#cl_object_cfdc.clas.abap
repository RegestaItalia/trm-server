CLASS /atrm/cl_object_cfdc DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_cfdc IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      lv_name  TYPE sobj_name,
      lv_where TYPE string.

    lv_name = me->key-obj_name.
    REPLACE ALL OCCURRENCES OF '''' IN lv_name WITH ''''''.

    CONCATENATE 'CDS_VIEW_NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_CDS'
        where_clause = lv_where
        object_field = 'CDS_VIEW_NAME'
        object_type  = 'CDS'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'CDS_VIEW_NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_CDS'
        where_clause = lv_where
        object_field = 'UNDERLYING_VIEW_NAME'
        object_type  = 'CDS'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'CDS_VIEW_NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_CDS'
        where_clause = lv_where
        object_field = 'BADI_ADDITIONAL_DATA_CLASS'
        object_type  = 'CLAS'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'CDS_VIEW_NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_CDS'
        where_clause = lv_where
        object_field = 'BADI_ADDITIONAL_DATA_CLS_CALC'
        object_type  = 'CLAS'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'CDS_VIEW_NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_CDS'
        where_clause = lv_where
        object_field = 'BADI_DEFINITION_FOR_CALC_FLDS'
        object_type  = 'ENHS'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'CDS_VIEW_NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_CDS'
        where_clause = lv_where
        object_field = 'BADI_DEFINITION_FOR_HIDDEN'
        object_type  = 'ENHS'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'CDS_VIEW_NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_CDS'
        where_clause = lv_where
        object_field = 'BEHAVIOR_DEFINITION'
        object_type  = 'BDEF'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'CDS_VIEW_NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_CDS'
        where_clause = lv_where
        object_field = 'CALCULATED_FIELDS_INCLUDE'
        object_type  = 'TABL'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'CDS_VIEW_NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_CDS'
        where_clause = lv_where
        object_field = 'ENTITY_IMPORT_PARAMETER_STRUC'
        object_type  = 'TABL'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'CDS_VIEW_NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_CDS'
        where_clause = lv_where
        object_field = 'ENTITY_IMPORT_PARA_STRUC_CALC'
        object_type  = 'TABL'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'CDS_VIEW_NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_CDS_CXT'
        where_clause = lv_where
        object_field = 'BUSINESS_CONTEXT'
        object_type  = 'CFDB'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'CDS_VIEW_NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_CDS_CXT'
        where_clause = lv_where
        object_field = 'UNDERLYING_VIEW_NAME'
        object_type  = 'CDS'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'CDS_VIEW_NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_CDS_CXT'
        where_clause = lv_where
        object_field = 'EXTENSION_INCL_VIEW_NAME'
        object_type  = 'CDS'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'CDS_VIEW_NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_CDS_CXT'
        where_clause = lv_where
        object_field = 'CH_GENERIC_PERS_VIEW_NAME'
        object_type  = 'CDS'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'CDS_VIEW_NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_CDS_CXT'
        where_clause = lv_where
        object_field = 'ROLE_TEXT_DATA_ELEMENT'
        object_type  = 'DTEL'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'CDS_VIEW_NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_DE_CDS_ASS'
        where_clause = lv_where
        object_field = 'TARGET_CDS_VIEW'
        object_type  = 'CDS'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'CDS_VIEW_NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_CDS_SUC'
        where_clause = lv_where
        object_field = 'SUCCESSOR'
        object_type  = 'CDS'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'CDS_VIEW_NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_DE_CDS_SUC'
        where_clause = lv_where
        object_field = 'SUCCESSOR'
        object_type  = 'CDS'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'CDS_VIEW_NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_CDS_REL_EN'
        where_clause = lv_where
        object_field = 'RELATED_CDS_VIEW'
        object_type  = 'CDS'
      CHANGING
        dependencies = dependencies.
  ENDMETHOD.

ENDCLASS.
