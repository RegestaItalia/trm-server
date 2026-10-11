CLASS /atrm/cl_object_bobf DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_bobf IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      lr_row         TYPE REF TO data,
      lv_table       TYPE tabname,
      lv_name        TYPE sobj_name,
      lv_bo_key      TYPE string,
      lv_where       TYPE string,
      ls_dependency  TYPE /atrm/object_dependency.

    FIELD-SYMBOLS:
      <ls_row>       TYPE any,
      <lv_value>     TYPE any.

    lv_name = me->key-obj_name.
    REPLACE ALL OCCURRENCES OF '''' IN lv_name WITH ''''''.

    TRY.
        lv_table = '/BOBF/OBM_BO'.
        CREATE DATA lr_row TYPE (lv_table).
        ASSIGN lr_row->* TO <ls_row>.
        CONCATENATE 'BO_NAME = ''' lv_name '''' INTO lv_where.
        SELECT SINGLE * FROM (lv_table) INTO <ls_row> WHERE (lv_where).
        CHECK sy-subrc = 0.

        ASSIGN COMPONENT 'BO_KEY' OF STRUCTURE <ls_row> TO <lv_value>.
        CHECK sy-subrc = 0.
        lv_bo_key = <lv_value>.

        ASSIGN COMPONENT 'SUPER_BO_NAME' OF STRUCTURE <ls_row> TO <lv_value>.
        IF sy-subrc = 0 AND <lv_value> IS NOT INITIAL.
          TRY.
              CALL METHOD get_tadir_dependency
                EXPORTING object = 'BOBF' obj_name = <lv_value>
                RECEIVING dependency = ls_dependency.
              IF ls_dependency IS NOT INITIAL.
                APPEND ls_dependency TO dependencies.
              ENDIF.
            CATCH cx_root.
              " optional base business object may not exist
          ENDTRY.
        ENDIF.

        REPLACE ALL OCCURRENCES OF '''' IN lv_bo_key WITH ''''''.
        CONCATENATE 'BO_KEY = ''' lv_bo_key '''' INTO lv_where.
        CALL METHOD append_table_dependencies EXPORTING table_name = '/BOBF/OBM_OBJ' where_clause = lv_where object_field = 'SP_MAPPER_CLASS' object_type = 'CLAS' CHANGING dependencies = dependencies.
        CALL METHOD append_table_dependencies EXPORTING table_name = '/BOBF/OBM_OBJ' where_clause = lv_where object_field = 'DERIVATOR_CLASS' object_type = 'CLAS' CHANGING dependencies = dependencies.
        CALL METHOD append_table_dependencies EXPORTING table_name = '/BOBF/OBM_OBJ' where_clause = lv_where object_field = 'STATUS_CLASS' object_type = 'CLAS' CHANGING dependencies = dependencies.
        CALL METHOD append_table_dependencies EXPORTING table_name = '/BOBF/OBM_OBJ' where_clause = lv_where object_field = 'ACCESS_CLASS' object_type = 'CLAS' CHANGING dependencies = dependencies.
        CALL METHOD append_table_dependencies EXPORTING table_name = '/BOBF/OBM_OBJ' where_clause = lv_where object_field = 'BUFFER_CLASS' object_type = 'CLAS' CHANGING dependencies = dependencies.
        CALL METHOD append_table_dependencies EXPORTING table_name = '/BOBF/OBM_OBJ' where_clause = lv_where object_field = 'MAPPER_CLASS' object_type = 'CLAS' CHANGING dependencies = dependencies.
        CALL METHOD append_table_dependencies EXPORTING table_name = '/BOBF/OBM_OBJ' where_clause = lv_where object_field = 'SP_CLASS' object_type = 'CLAS' CHANGING dependencies = dependencies.
        CALL METHOD append_table_dependencies EXPORTING table_name = '/BOBF/OBM_OBJ' where_clause = lv_where object_field = 'ES_DATA_EXTRACTOR_CLASS' object_type = 'CLAS' CHANGING dependencies = dependencies.
        CALL METHOD append_table_dependencies EXPORTING table_name = '/BOBF/OBM_OBJ' where_clause = lv_where object_field = 'CONST_INTERFACE' object_type = 'INTF' CHANGING dependencies = dependencies.
        CALL METHOD append_table_dependencies EXPORTING table_name = '/BOBF/OBM_OBJ' where_clause = lv_where object_field = 'OBJECT_MODEL_CDS_VIEW_NAME' object_type = 'CDS' CHANGING dependencies = dependencies.
        CALL METHOD append_table_dependencies EXPORTING table_name = '/BOBF/OBM_NODE' where_clause = lv_where object_field = 'BUF_CLASS' object_type = 'CLAS' CHANGING dependencies = dependencies.
        CALL METHOD append_table_dependencies EXPORTING table_name = '/BOBF/OBM_NODE' where_clause = lv_where object_field = 'MAPPER_CLASS' object_type = 'CLAS' CHANGING dependencies = dependencies.
        CALL METHOD append_table_dependencies EXPORTING table_name = '/BOBF/OBM_NODE' where_clause = lv_where object_field = 'SP_MAPPER_CLASS' object_type = 'CLAS' CHANGING dependencies = dependencies.
        CALL METHOD append_table_dependencies EXPORTING table_name = '/BOBF/OBM_NODE' where_clause = lv_where object_field = 'DELEGATION_CLASS' object_type = 'CLAS' CHANGING dependencies = dependencies.
        CALL METHOD append_table_dependencies EXPORTING table_name = '/BOBF/OBM_NODE' where_clause = lv_where object_field = 'LCP_WRAP_CLASS' object_type = 'CLAS' CHANGING dependencies = dependencies.
        CALL METHOD append_table_dependencies EXPORTING table_name = '/BOBF/OBM_NODE' where_clause = lv_where object_field = 'SP_ID_MAP_CLASS' object_type = 'CLAS' CHANGING dependencies = dependencies.
        CALL METHOD append_table_dependencies EXPORTING table_name = '/BOBF/OBM_NODE' where_clause = lv_where object_field = 'AUTH_CHECK_CLASS' object_type = 'CLAS' CHANGING dependencies = dependencies.
        CALL METHOD append_table_dependencies EXPORTING table_name = '/BOBF/OBM_NODE' where_clause = lv_where object_field = 'DRAFT_CLASS' object_type = 'CLAS' CHANGING dependencies = dependencies.
        CALL METHOD append_table_dependencies EXPORTING table_name = '/BOBF/OBM_NODE' where_clause = lv_where object_field = 'NODE_CLASS' object_type = 'CLAS' CHANGING dependencies = dependencies.
        CALL METHOD append_table_dependencies EXPORTING table_name = '/BOBF/OBM_NODE' where_clause = lv_where object_field = 'DATABASE_TABLE' object_type = 'TABL' CHANGING dependencies = dependencies.
        CALL METHOD append_table_dependencies EXPORTING table_name = '/BOBF/OBM_NODE' where_clause = lv_where object_field = 'DATA_DATA_TYPE' object_type = 'TABL' CHANGING dependencies = dependencies.
        CALL METHOD append_table_dependencies EXPORTING table_name = '/BOBF/OBM_NODE' where_clause = lv_where object_field = 'DATA_TABLE_TYPE' object_type = 'TTYP' CHANGING dependencies = dependencies.
        CALL METHOD append_table_dependencies EXPORTING table_name = '/BOBF/OBM_NODE' where_clause = lv_where object_field = 'EXT_INCL_NAME' object_type = 'TABL' CHANGING dependencies = dependencies.
        CALL METHOD append_table_dependencies EXPORTING table_name = '/BOBF/OBM_NODE' where_clause = lv_where object_field = 'OBJECT_MODEL_CDS_VIEW_NAME' object_type = 'CDS' CHANGING dependencies = dependencies.
        CALL METHOD append_table_dependencies EXPORTING table_name = '/BOBF/ACT_LIST' where_clause = lv_where object_field = 'ACT_CLASS' object_type = 'CLAS' CHANGING dependencies = dependencies.
        CALL METHOD append_table_dependencies EXPORTING table_name = '/BOBF/ACT_LIST' where_clause = lv_where object_field = 'PARAM_DATA_TYPE' object_type = 'TABL' CHANGING dependencies = dependencies.
        CALL METHOD append_table_dependencies EXPORTING table_name = '/BOBF/ACT_LIST' where_clause = lv_where object_field = 'PARAM_CDS_ABSTRACT_ENTITY' object_type = 'CDS' CHANGING dependencies = dependencies.
        CALL METHOD append_table_dependencies EXPORTING table_name = '/BOBF/ACT_LIST' where_clause = lv_where object_field = 'MSGID_CHECK' object_type = 'MSAG' CHANGING dependencies = dependencies.
        CALL METHOD append_table_dependencies EXPORTING table_name = '/BOBF/ACT_LIST' where_clause = lv_where object_field = 'MSGID_SUCCESS' object_type = 'MSAG' CHANGING dependencies = dependencies.
        CALL METHOD append_table_dependencies EXPORTING table_name = '/BOBF/ACT_LIST' where_clause = lv_where object_field = 'MSGID_ERROR' object_type = 'MSAG' CHANGING dependencies = dependencies.
        CALL METHOD append_table_dependencies EXPORTING table_name = '/BOBF/DET_LIST' where_clause = lv_where object_field = 'DET_CLASS' object_type = 'CLAS' CHANGING dependencies = dependencies.
        CALL METHOD append_table_dependencies EXPORTING table_name = '/BOBF/VAL_LIST' where_clause = lv_where object_field = 'VAL_CLASS' object_type = 'CLAS' CHANGING dependencies = dependencies.
        CALL METHOD append_table_dependencies EXPORTING table_name = '/BOBF/OBM_ASSOC' where_clause = lv_where object_field = 'ASSOC_CLASS' object_type = 'CLAS' CHANGING dependencies = dependencies.
        CALL METHOD append_table_dependencies EXPORTING table_name = '/BOBF/OBM_ASSOC' where_clause = lv_where object_field = 'PARAM_DATA_TYPE' object_type = 'TABL' CHANGING dependencies = dependencies.
        CALL METHOD append_table_dependencies EXPORTING table_name = '/BOBF/OBM_ASSOC' where_clause = lv_where object_field = 'SP_MAPPER_CLASS' object_type = 'CLAS' CHANGING dependencies = dependencies.
        CALL METHOD append_table_dependencies EXPORTING table_name = '/BOBF/OBM_QUERY' where_clause = lv_where object_field = 'QUERY_CLASS' object_type = 'CLAS' CHANGING dependencies = dependencies.
        CALL METHOD append_table_dependencies EXPORTING table_name = '/BOBF/OBM_QUERY' where_clause = lv_where object_field = 'DATA_TYPE' object_type = 'TABL' CHANGING dependencies = dependencies.
        CALL METHOD append_table_dependencies EXPORTING table_name = '/BOBF/OBM_QUERY' where_clause = lv_where object_field = 'RESULT_TYPE' object_type = 'TABL' CHANGING dependencies = dependencies.
        CALL METHOD append_table_dependencies EXPORTING table_name = '/BOBF/OBM_ALTKEY' where_clause = lv_where object_field = 'DATA_TYPE' object_type = 'TABL' CHANGING dependencies = dependencies.
        CALL METHOD append_table_dependencies EXPORTING table_name = '/BOBF/OBM_ALTKEY' where_clause = lv_where object_field = 'DATA_TABLE_TYPE' object_type = 'TTYP' CHANGING dependencies = dependencies.
        CALL METHOD append_table_dependencies EXPORTING table_name = '/BOBF/STA_DERIV' where_clause = lv_where object_field = 'DERIVATOR_CLASS' object_type = 'CLAS' CHANGING dependencies = dependencies.
      CATCH cx_root.
        " BOPF repository APIs may not exist in the target system
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
