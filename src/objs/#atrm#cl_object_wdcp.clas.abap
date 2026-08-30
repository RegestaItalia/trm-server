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
    DATA:
      ls_object_type TYPE wbobjtype,
      lo_operator    TYPE REF TO object,
      lo_data_model  TYPE REF TO object,
      ls_chip        TYPE wdy_chip_def_xt_ui,
      lv_config      TYPE sobj_name,
      ls_dependency  TYPE /atrm/object_dependency.

    TRY.
        ls_object_type-objtype_tr = 'WDCP'.
        ls_object_type-subtype_wb = 'YP'.

        CALL METHOD ('CL_WB_OBJECT_OPERATOR')=>('CREATE_INSTANCE')
          EXPORTING object_type = ls_object_type object_key = me->key-obj_name
          RECEIVING result = lo_operator.
        CALL METHOD lo_operator->('IF_WB_OBJECT_OPERATOR~READ')
          EXPORTING version = 'A' data_selection = 'AL'
          IMPORTING eo_object_data = lo_data_model.
        CALL METHOD lo_data_model->('IF_WB_OBJECT_DATA_MODEL~GET_DATA')
          IMPORTING p_data = ls_chip.

        IF ls_chip-component IS NOT INITIAL.
          TRY.
              CALL METHOD get_tadir_dependency
                EXPORTING object = 'WDYN' obj_name = ls_chip-component
                RECEIVING dependency = ls_dependency.
              APPEND ls_dependency TO dependencies.
            CATCH cx_root.
              " optional dependency may not exist in the target system
          ENDTRY.
        ENDIF.

        IF ls_chip-config_id IS NOT INITIAL.
          CONCATENATE
            ls_chip-config_id
            ls_chip-config_type
            ls_chip-config_var
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

        IF ls_chip-remote_chip_appl IS NOT INITIAL.
          TRY.
              CLEAR ls_dependency.
              CALL METHOD get_tadir_dependency
                EXPORTING object = 'WDYA' obj_name = ls_chip-remote_chip_appl
                RECEIVING dependency = ls_dependency.
              APPEND ls_dependency TO dependencies.
            CATCH cx_root.
              " optional dependency may not exist in the target system
          ENDTRY.
        ENDIF.

        IF ls_chip-config_idcfg IS NOT INITIAL.
          CLEAR lv_config.
          CONCATENATE
            ls_chip-config_idcfg
            ls_chip-config_typecfg
            ls_chip-config_varcfg
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
      CATCH cx_root.
        " optional Web Dynpro CHIP API may not exist in the target system
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
