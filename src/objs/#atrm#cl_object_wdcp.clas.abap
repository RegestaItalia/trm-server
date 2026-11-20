CLASS /atrm/cl_object_wdcp DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_wdcp IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    FIELD-SYMBOLS: <ls_object_type> TYPE any,
                   <lv_wb_type_part> TYPE any,
                   <ls_chip>         TYPE any,
                   <lv_component>    TYPE any,
                   <lv_config_id>    TYPE any,
                   <lv_config_type>  TYPE any,
                   <lv_config_var>   TYPE any.
    DATA:
      lr_object_type TYPE REF TO data,
      lr_chip        TYPE REF TO data,
      lo_operator    TYPE REF TO object,
      lo_data_model  TYPE REF TO object,
      lv_obj_name    TYPE sobj_name,
      lv_config      TYPE sobj_name,
      ls_dependency  TYPE /atrm/object_dependency.

    TRY.
        CREATE DATA lr_object_type TYPE ('WBOBJTYPE').
        ASSIGN lr_object_type->* TO <ls_object_type>.
        ASSIGN COMPONENT 'OBJTYPE_TR' OF STRUCTURE <ls_object_type> TO <lv_wb_type_part>.
        <lv_wb_type_part> = 'WDCP'.
        ASSIGN COMPONENT 'SUBTYPE_WB' OF STRUCTURE <ls_object_type> TO <lv_wb_type_part>.
        <lv_wb_type_part> = 'YP'.

        " WDY_CHIP_DEF_XT_UI does not exist on every release (e.g. SAP_BASIS 700)
        CREATE DATA lr_chip TYPE ('WDY_CHIP_DEF_XT_UI').
        ASSIGN lr_chip->* TO <ls_chip>.

        CALL METHOD ('CL_WB_OBJECT_OPERATOR')=>('CREATE_INSTANCE')
          EXPORTING object_type = <ls_object_type> object_key = me->key-obj_name
          RECEIVING result = lo_operator.
        CALL METHOD lo_operator->('IF_WB_OBJECT_OPERATOR~READ')
          EXPORTING version = 'A' data_selection = 'AL'
          IMPORTING eo_object_data = lo_data_model.
        CALL METHOD lo_data_model->('IF_WB_OBJECT_DATA_MODEL~GET_DATA')
          IMPORTING p_data = <ls_chip>.

        ASSIGN COMPONENT 'COMPONENT' OF STRUCTURE <ls_chip> TO <lv_component>.
        IF sy-subrc = 0 AND <lv_component> IS NOT INITIAL.
          lv_obj_name = <lv_component>.
          TRY.
              CALL METHOD get_tadir_dependency
                EXPORTING object = 'WDYN' obj_name = lv_obj_name
                RECEIVING dependency = ls_dependency.
              APPEND ls_dependency TO dependencies.
            CATCH cx_root.
              " optional dependency may not exist in the target system
          ENDTRY.
        ENDIF.

        ASSIGN COMPONENT 'CONFIG_ID' OF STRUCTURE <ls_chip> TO <lv_config_id>.
        IF sy-subrc = 0 AND <lv_config_id> IS NOT INITIAL.
          ASSIGN COMPONENT 'CONFIG_TYPE' OF STRUCTURE <ls_chip> TO <lv_config_type>.
          IF sy-subrc = 0.
            ASSIGN COMPONENT 'CONFIG_VAR' OF STRUCTURE <ls_chip> TO <lv_config_var>.
          ENDIF.
          IF sy-subrc = 0.
            CLEAR lv_config.
            CONCATENATE
              <lv_config_id>
              <lv_config_type>
              <lv_config_var>
              INTO lv_config.
            TRY.
                CLEAR ls_dependency.
                CALL METHOD get_tadir_dependency
                  EXPORTING object = 'WDCC' obj_name = lv_config
                  RECEIVING dependency = ls_dependency.
                APPEND ls_dependency TO dependencies.
              CATCH cx_root.
                " optional dependency may not exist in the target system
            ENDTRY.
          ENDIF.
        ENDIF.

        ASSIGN COMPONENT 'REMOTE_CHIP_APPL' OF STRUCTURE <ls_chip> TO <lv_component>.
        IF sy-subrc = 0 AND <lv_component> IS NOT INITIAL.
          lv_obj_name = <lv_component>.
          TRY.
              CLEAR ls_dependency.
              CALL METHOD get_tadir_dependency
                EXPORTING object = 'WDYA' obj_name = lv_obj_name
                RECEIVING dependency = ls_dependency.
              APPEND ls_dependency TO dependencies.
            CATCH cx_root.
              " optional dependency may not exist in the target system
          ENDTRY.
        ENDIF.

        ASSIGN COMPONENT 'CONFIG_IDCFG' OF STRUCTURE <ls_chip> TO <lv_config_id>.
        IF sy-subrc = 0 AND <lv_config_id> IS NOT INITIAL.
          ASSIGN COMPONENT 'CONFIG_TYPECFG' OF STRUCTURE <ls_chip> TO <lv_config_type>.
          IF sy-subrc = 0.
            ASSIGN COMPONENT 'CONFIG_VARCFG' OF STRUCTURE <ls_chip> TO <lv_config_var>.
          ENDIF.
          IF sy-subrc = 0.
            CLEAR lv_config.
            CONCATENATE
              <lv_config_id>
              <lv_config_type>
              <lv_config_var>
              INTO lv_config.
            TRY.
                CLEAR ls_dependency.
                CALL METHOD get_tadir_dependency
                  EXPORTING object = 'WDCC' obj_name = lv_config
                  RECEIVING dependency = ls_dependency.
                APPEND ls_dependency TO dependencies.
              CATCH cx_root.
                " optional dependency may not exist in the target system
            ENDTRY.
          ENDIF.
        ENDIF.
      CATCH cx_root.
        " optional Web Dynpro CHIP API/types may not exist in the target system
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

