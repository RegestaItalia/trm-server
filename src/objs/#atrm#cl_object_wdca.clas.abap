CLASS /atrm/cl_object_wdca DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_wdca IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      lo_config     TYPE REF TO object,
      ls_config_key TYPE wdy_config_key,
      lv_object     TYPE wdy_md_object_name,
      ls_outline    TYPE wdy_cfg_outline_data,
      lt_data       TYPE wdy_cfg_persist_data_appl_tab,
      ls_data       LIKE LINE OF lt_data,
      ls_component  TYPE wdy_cfg_component_hier,
      lv_config     TYPE sobj_name,
      ls_dependency TYPE /atrm/object_dependency.

    TRY.
        ls_config_key = me->key-obj_name.

        CREATE OBJECT lo_config
          TYPE ('CL_WDR_CFG_PERSISTENCE_APPL')
          EXPORTING
            config_key  = ls_config_key
            object_name = lv_object.

        CALL METHOD lo_config->('READ_OUTLINE_DATA')
          RECEIVING
            r_outline_data = ls_outline.
        CALL METHOD lo_config->('READ_DATA')
          RECEIVING
            application_data = lt_data.

        IF ls_outline-object_name IS NOT INITIAL.
          TRY.
              CALL METHOD get_tadir_dependency
                EXPORTING object = 'WDYA' obj_name = ls_outline-object_name
                RECEIVING dependency = ls_dependency.
              APPEND ls_dependency TO dependencies.
            CATCH cx_root.
              " optional dependency may not exist in the target system
          ENDTRY.
        ENDIF.

        LOOP AT lt_data INTO ls_data.
          LOOP AT ls_data-cmp_hierarchy INTO ls_component.
            IF ls_component-component_name IS NOT INITIAL.
              TRY.
                  CLEAR ls_dependency.
                  CALL METHOD get_tadir_dependency
                    EXPORTING object = 'WDYN' obj_name = ls_component-component_name
                    RECEIVING dependency = ls_dependency.
                  APPEND ls_dependency TO dependencies.
                CATCH cx_root.
                  " optional dependency may not exist in the target system
              ENDTRY.
            ENDIF.

            IF ls_component-config_id IS NOT INITIAL.
              CONCATENATE
                ls_component-config_id
                ls_component-config_type
                ls_component-config_var
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
          ENDLOOP.
        ENDLOOP.
      CATCH cx_root.
        " optional Web Dynpro configuration API may not exist in the target system
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
