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
      ls_config_key TYPE wdy_config_key,
      ls_config     TYPE wdy_config_data,
      lv_xml        TYPE xstring,
      lv_parent     TYPE sobj_name,
      ls_dependency TYPE /atrm/object_dependency.

    TRY.
        ls_config_key = me->key-obj_name.

        CALL METHOD ('CL_WDR_CFG_PERSISTENCE_UTILS')=>('READ_COMP_CONFIG_FROM_DB')
          EXPORTING
            config_key           = ls_config_key
          IMPORTING
            xml_xcontent         = lv_xml
            original_config_data = ls_config.

        IF ls_config-component IS NOT INITIAL.
          TRY.
              CALL METHOD get_tadir_dependency
                EXPORTING object = 'WDYN' obj_name = ls_config-component
                RECEIVING dependency = ls_dependency.
              APPEND ls_dependency TO dependencies.
            CATCH cx_root.
              " optional dependency may not exist in the target system
          ENDTRY.
        ENDIF.

        IF ls_config-config_idpar IS NOT INITIAL.
          CONCATENATE
            ls_config-config_idpar
            ls_config-config_typepar
            ls_config-config_varpar
            INTO lv_parent.
          TRY.
              CLEAR ls_dependency.
              CALL METHOD get_tadir_dependency
                EXPORTING object = 'WDCC' obj_name = lv_parent
                RECEIVING dependency = ls_dependency.
              APPEND ls_dependency TO dependencies.
            CATCH cx_root.
              " optional dependency may not exist in the target system
          ENDTRY.
        ENDIF.
      CATCH cx_root.
        " optional Web Dynpro configuration API may not exist in the target system
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
