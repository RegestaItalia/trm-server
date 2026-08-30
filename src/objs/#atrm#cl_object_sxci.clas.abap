CLASS /atrm/cl_object_sxci DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_sxci IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      lv_impl_name TYPE rsexscrn-imp_name,
      lv_exit_name TYPE rsexscrn-exit_name,
      lv_impl_class TYPE seoclsname,
      lv_interface TYPE seoclsname,
      lv_enhancement TYPE sobj_name,
      ls_dependency TYPE /atrm/object_dependency.

    TRY.
        lv_impl_name = me->key-obj_name.

        CALL FUNCTION 'SXV_EXIT_FOR_IMP'
          EXPORTING
            imp_name = lv_impl_name
          IMPORTING
            exit_name = lv_exit_name
          EXCEPTIONS
            data_inconsistency = 1
            OTHERS = 2.

        IF sy-subrc = 0 AND lv_exit_name IS NOT INITIAL.
          TRY.
              CALL METHOD get_tadir_dependency
                EXPORTING object = 'SXSD' obj_name = lv_exit_name
                RECEIVING dependency = ls_dependency.
              APPEND ls_dependency TO dependencies.
            CATCH cx_root.
              " optional dependency may not exist in the target system
          ENDTRY.
        ENDIF.

        SELECT SINGLE imp_class inter_name
          FROM sxc_class
          INTO (lv_impl_class, lv_interface)
          WHERE imp_name = lv_impl_name.

        IF lv_impl_class IS NOT INITIAL.
          TRY.
              CLEAR ls_dependency.
              CALL METHOD get_tadir_dependency
                EXPORTING object = 'CLAS' obj_name = lv_impl_class
                RECEIVING dependency = ls_dependency.
              APPEND ls_dependency TO dependencies.
            CATCH cx_root.
              " optional dependency may not exist in the target system
          ENDTRY.
        ENDIF.

        IF lv_interface IS NOT INITIAL.
          TRY.
              CLEAR ls_dependency.
              CALL METHOD get_tadir_dependency
                EXPORTING object = 'INTF' obj_name = lv_interface
                RECEIVING dependency = ls_dependency.
              APPEND ls_dependency TO dependencies.
            CATCH cx_root.
              " optional dependency may not exist in the target system
          ENDTRY.
        ENDIF.

        SELECT SINGLE mig_enhname
          FROM sxc_attr
          INTO lv_enhancement
          WHERE imp_name = lv_impl_name
            AND version = 'A'.
        IF lv_enhancement IS NOT INITIAL.
          TRY.
              CLEAR ls_dependency.
              CALL METHOD get_tadir_dependency
                EXPORTING object = 'ENHO' obj_name = lv_enhancement
                RECEIVING dependency = ls_dependency.
              APPEND ls_dependency TO dependencies.
            CATCH cx_root.
              " optional dependency may not exist in the target system
          ENDTRY.
        ENDIF.
      CATCH cx_root.
        " optional classic BAdI API may not exist in the target system
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
