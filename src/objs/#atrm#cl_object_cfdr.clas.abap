CLASS /atrm/cl_object_cfdr DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_cfdr IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      lv_name  TYPE sobj_name,
      lv_where TYPE string.

    lv_name = me->key-obj_name.
    REPLACE ALL OCCURRENCES OF '''' IN lv_name WITH ''''''.

    CONCATENATE 'ASSOCIATION_TARGET_NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_STD_BO'
        where_clause = lv_where
        object_field = 'CDS_VALUE_HELP_VIEW_NAME'
        object_type  = 'CDS'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'ASSOCIATION_TARGET_NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_STD_BO'
        where_clause = lv_where
        object_field = 'LEADING_DIMENSION_VIEW_NAME'
        object_type  = 'CDS'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'ASSOCIATION_TARGET_NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_STD_BO'
        where_clause = lv_where
        object_field = 'BADI_IMPL_FOR_DATA_SUBJECT'
        object_type  = 'SXCI'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'ASSOCIATION_TARGET_NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_STD_BO'
        where_clause = lv_where
        object_field = 'IRF_DATA_MAPPING_CLASS'
        object_type  = 'CLAS'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'ASSOCIATION_TARGET_NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_STD_BO'
        where_clause = lv_where
        object_field = 'GUI_SEARCH_HELP_NAME'
        object_type  = 'SHLP'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'ASSOCIATION_TARGET_NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_STD_BO_F'
        where_clause = lv_where
        object_field = 'DIMENSION_VIEW_NAME'
        object_type  = 'CDS'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'ASSOCIATION_TARGET_NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_STD_BO_F'
        where_clause = lv_where
        object_field = 'DOMAIN_NAME'
        object_type  = 'DOMA'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'ASSOCIATION_TARGET_NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_STD_BO_F'
        where_clause = lv_where
        object_field = 'TEXT_DOMAIN_NAME'
        object_type  = 'DOMA'
      CHANGING
        dependencies = dependencies.
  ENDMETHOD.

ENDCLASS.
