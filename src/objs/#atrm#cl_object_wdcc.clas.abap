CLASS /atrm/cl_object_wdcc DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_wdcc IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      lv_table      TYPE tabname,
      lv_where      TYPE string,
      lv_wul_where  TYPE string,
      ls_config_key TYPE wdy_config_key,
      lv_name       TYPE sobj_name,
      lr_rows       TYPE REF TO data,
      lo_analyzer   TYPE REF TO if_wdr_cfg_analyze_config_data,
      lo_appl       TYPE REF TO if_wdr_cfg_analyze_config_appl,
      lt_types      TYPE if_wdr_cfg_analyze_config_data=>ty_t_types,
      ls_type       LIKE LINE OF lt_types,
      lt_attributes TYPE if_wdr_cfg_analyze_config_data=>ty_t_attribute_values,
      ls_attribute  LIKE LINE OF lt_attributes,
      ls_value      TYPE if_wdr_cfg_analyze_config_data=>ty_s_name_value,
      ls_dependency TYPE /atrm/object_dependency.

    FIELD-SYMBOLS:
      <lt_rows>  TYPE STANDARD TABLE,
      <ls_row>   TYPE any,
      <lv_id>    TYPE any,
      <lv_type>  TYPE any,
      <lv_var>   TYPE any,
      <lv_value> TYPE any.

    ls_config_key = me->key-obj_name.
    lv_name = ls_config_key-config_id.
    REPLACE ALL OCCURRENCES OF '''' IN lv_name WITH ''''''.
    CONCATENATE 'CONFIG_ID = ''' lv_name ''' AND CONFIG_TYPE = ''' ls_config_key-config_type
                ''' AND CONFIG_VAR = ''' ls_config_key-config_var '''' INTO lv_where.
    CONCATENATE lv_where ' AND ENHNAME = '' ''' INTO lv_wul_where RESPECTING BLANKS.

    " Configured component
    append_table_dependencies(
      EXPORTING table_name   = 'WDY_CONFIG_DATA'
                where_clause = lv_where
                object_field = 'COMPONENT'
                object_type  = 'WDYN'
      CHANGING  dependencies = dependencies ).

    " Used configurations (parent configuration, embedded UIBB configurations) and components
    TRY.
        lv_table = 'WDY_CFG_CMP_WUL'.
        CREATE DATA lr_rows TYPE STANDARD TABLE OF (lv_table).
        ASSIGN lr_rows->* TO <lt_rows>.
        SELECT * FROM (lv_table) INTO TABLE <lt_rows> WHERE (lv_wul_where).
        LOOP AT <lt_rows> ASSIGNING <ls_row>.
          ASSIGN COMPONENT 'USED_CONFIG_ID' OF STRUCTURE <ls_row> TO <lv_id>.
          ASSIGN COMPONENT 'USED_CONFIG_TYPE' OF STRUCTURE <ls_row> TO <lv_type>.
          ASSIGN COMPONENT 'USED_CONFIG_VAR' OF STRUCTURE <ls_row> TO <lv_var>.
          IF <lv_id> IS ASSIGNED AND <lv_id> IS NOT INITIAL.
            CLEAR ls_config_key.
            ls_config_key-config_id = <lv_id>.
            ls_config_key-config_type = <lv_type>.
            ls_config_key-config_var = <lv_var>.
            lv_name = ls_config_key.
            TRY.
                CLEAR ls_dependency.
                CALL METHOD get_tadir_dependency
                  EXPORTING object = 'WDCC' obj_name = lv_name
                  RECEIVING dependency = ls_dependency.
                APPEND ls_dependency TO dependencies.
              CATCH cx_root.
                " optional dependency may not exist in the target system
            ENDTRY.
          ENDIF.
          ASSIGN COMPONENT 'USED_COMPONENT' OF STRUCTURE <ls_row> TO <lv_value>.
          IF sy-subrc = 0 AND <lv_value> IS NOT INITIAL.
            TRY.
                CLEAR ls_dependency.
                CALL METHOD get_tadir_dependency
                  EXPORTING object = 'WDYN' obj_name = <lv_value>
                  RECEIVING dependency = ls_dependency.
                APPEND ls_dependency TO dependencies.
              CATCH cx_root.
                " optional dependency may not exist in the target system
            ENDTRY.
          ENDIF.
          UNASSIGN: <lv_id>, <lv_type>, <lv_var>.
        ENDLOOP.
      CATCH cx_root.
        " optional repository table may not exist in the target system
    ENDTRY.

    " Components and feeder classes typed in the configuration content (FPM UIBBs)
    TRY.
        ls_config_key = me->key-obj_name.
        cl_wdr_cfg_analyze_config_data=>create_for_config_key(
          EXPORTING config_key           = ls_config_key
          IMPORTING config_data_analyzer = lo_analyzer
                    config_appl_analyzer = lo_appl ).
        IF lo_analyzer IS BOUND.
          ls_type-type_kind = if_wdr_cfg_analyze_config_data=>co_type_kind-ddic_rollname.
          ls_type-type_name = 'FPM_COMPONENT_NAME'.
          INSERT ls_type INTO TABLE lt_types.
          ls_type-type_name = 'WDY_COMPONENT_NAME'.
          INSERT ls_type INTO TABLE lt_types.
          ls_type-type_name = 'SEOCLSNAME'.
          INSERT ls_type INTO TABLE lt_types.
          lt_attributes = lo_analyzer->get_attributes_for_type( lt_types ).
        ENDIF.
        LOOP AT lt_attributes INTO ls_attribute.
          LOOP AT ls_attribute-attributes INTO ls_value WHERE value IS NOT INITIAL.
            lv_name = ls_value-value.
            TRANSLATE lv_name TO UPPER CASE.
            TRY.
                CLEAR ls_dependency.
                IF ls_value-type-type_name = 'SEOCLSNAME'.
                  TRY.
                      CALL METHOD get_tadir_dependency
                        EXPORTING object = 'CLAS' obj_name = lv_name
                        RECEIVING dependency = ls_dependency.
                    CATCH cx_root.
                      CALL METHOD get_tadir_dependency
                        EXPORTING object = 'INTF' obj_name = lv_name
                        RECEIVING dependency = ls_dependency.
                  ENDTRY.
                ELSE.
                  CALL METHOD get_tadir_dependency
                    EXPORTING object = 'WDYN' obj_name = lv_name
                    RECEIVING dependency = ls_dependency.
                ENDIF.
                APPEND ls_dependency TO dependencies.
              CATCH cx_root.
                " optional dependency may not exist in the target system
            ENDTRY.
          ENDLOOP.
        ENDLOOP.
      CATCH cx_root.
        " optional Web Dynpro configuration API may not exist in the target system
    ENDTRY.

    SORT dependencies BY tabname tabkey.
    DELETE ADJACENT DUPLICATES FROM dependencies COMPARING tabname tabkey.
  ENDMETHOD.

ENDCLASS.
