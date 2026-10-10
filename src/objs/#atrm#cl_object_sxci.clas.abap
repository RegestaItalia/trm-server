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
      lv_name TYPE string,
      lv_where TYPE string,
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
      CATCH cx_root.
        " optional classic BAdI API may not exist in the target system
    ENDTRY.

    lv_name = me->key-obj_name.
    REPLACE ALL OCCURRENCES OF `'` IN lv_name WITH `''`.

    " Migration enhancement implementation (active rows have VERSION blank or A)
    CONCATENATE `IMP_NAME = '` lv_name `' AND ( VERSION = ' ' OR VERSION = 'A' )` INTO lv_where.
    append_table_dependencies(
      EXPORTING table_name   = 'SXC_ATTR'
                where_clause = lv_where
                object_field = 'MIG_ENHNAME'
                object_type  = 'ENHO'
      CHANGING  dependencies = dependencies ).

    " Subscreen implementing program
    CONCATENATE `IMP_NAME = '` lv_name `'` INTO lv_where.
    append_table_dependencies(
      EXPORTING table_name   = 'SXC_SCRN'
                where_clause = lv_where
                object_field = 'SCR_P_PROG'
                object_type  = 'PROG'
      CHANGING  dependencies = dependencies ).

    SORT dependencies BY tabname tabkey.
    DELETE ADJACENT DUPLICATES FROM dependencies COMPARING tabname tabkey.
  ENDMETHOD.

ENDCLASS.
