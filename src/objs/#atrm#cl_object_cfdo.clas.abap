CLASS /atrm/cl_object_cfdo DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_cfdo IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      lv_name  TYPE sobj_name,
      lv_where TYPE string.

    lv_name = me->key-obj_name.
    REPLACE ALL OCCURRENCES OF '''' IN lv_name WITH ''''''.

    CONCATENATE 'MODEL_NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_ODATA'
        where_clause = lv_where
        object_field = 'MODEL_NAME'
        object_type  = 'IWMO'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'MODEL_NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_ODATA_CTXT'
        where_clause = lv_where
        object_field = 'BUSINESS_CONTEXT'
        object_type  = 'CFDB'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'MODEL_NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_ODATA_CTXT'
        where_clause = lv_where
        object_field = 'CDS_VIEW_NAME'
        object_type  = 'CDS'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'MODEL_NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_ODATA_CTXT'
        where_clause = lv_where
        object_field = 'ROLE_FIELDS_INCLUDE'
        object_type  = 'TABL'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'MODEL_NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_ODATA_CTXT'
        where_clause = lv_where
        object_field = 'UNION_FIELDS_INCLUDE'
        object_type  = 'TABL'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'MODEL_NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_ODATA_CTXT'
        where_clause = lv_where
        object_field = 'ROLE_TEXT_DATA_ELEMENT'
        object_type  = 'DTEL'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'MODEL_NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_DE_ODATA_C'
        where_clause = lv_where
        object_field = 'CDS_VIEW_NAME'
        object_type  = 'CDS'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'MODEL_NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_DE_ODATA_C'
        where_clause = lv_where
        object_field = 'EXTENDED_DDIC_STRUCTURE'
        object_type  = 'TABL'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'MODEL_NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_ODATA_SUC'
        where_clause = lv_where
        object_field = 'SUCCESSOR'
        object_type  = 'CFDO'
      CHANGING
        dependencies = dependencies.

    CONCATENATE 'MODEL_NAME = ''' lv_name '''' INTO lv_where.
    CALL METHOD append_table_dependencies
      EXPORTING
        table_name   = 'CFD_W_DE_ODATA_S'
        where_clause = lv_where
        object_field = 'SUCCESSOR'
        object_type  = 'CFDO'
      CHANGING
        dependencies = dependencies.
  ENDMETHOD.

ENDCLASS.
