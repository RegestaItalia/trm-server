CLASS /atrm/cl_object_ensc DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS /atrm/cl_object_ensc IMPLEMENTATION.
  METHOD /atrm/if_object~get_dependencies.
    DATA:
      lv_table_name TYPE tabname,
      lv_object_name TYPE sobj_name,
      ls_dependency TYPE /atrm/object_dependency.
    TRY.
        lv_table_name = 'ENHSPOTCOMPSPOT'.
        SELECT childspot FROM (lv_table_name) INTO lv_object_name
          WHERE enhspotcomposite = me->key-obj_name AND version = 'A'.
          IF lv_object_name IS NOT INITIAL.
            TRY.
                CLEAR ls_dependency.
                CALL METHOD get_tadir_dependency
                  EXPORTING object = 'ENHS' obj_name = lv_object_name
                  RECEIVING dependency = ls_dependency.
                APPEND ls_dependency TO dependencies.
              CATCH cx_root.
            ENDTRY.
          ENDIF.
        ENDSELECT.
      CATCH cx_root.
    ENDTRY.
    TRY.
        lv_table_name = 'ENHSPOTCOMPCOMP'.
        SELECT childcomposite FROM (lv_table_name) INTO lv_object_name
          WHERE enhspotcomposite = me->key-obj_name AND version = 'A'.
          IF lv_object_name IS NOT INITIAL.
            TRY.
                CLEAR ls_dependency.
                CALL METHOD get_tadir_dependency
                  EXPORTING object = 'ENSC' obj_name = lv_object_name
                  RECEIVING dependency = ls_dependency.
                APPEND ls_dependency TO dependencies.
              CATCH cx_root.
            ENDTRY.
          ENDIF.
        ENDSELECT.
      CATCH cx_root.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
