CLASS /atrm/cl_object_sxsd DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_sxsd IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      lv_exit_name  TYPE rsexscrn-exit_name,
      ls_badi       TYPE badi_data,
      lv_ext_class  TYPE seoclsname,
      lv_name       TYPE string,
      lv_where      TYPE string,
      ls_dependency TYPE /atrm/object_dependency.

    TRY.
        lv_exit_name = me->key-obj_name.

        CALL FUNCTION 'SXO_BADI_READ'
          EXPORTING
            exit_name    = lv_exit_name
          IMPORTING
            badi         = ls_badi
            ext_clname   = lv_ext_class
          EXCEPTIONS
            read_failure = 1
            OTHERS       = 2.
        IF sy-subrc = 0.

          IF ls_badi-inter_name IS NOT INITIAL.
            TRY.
                CALL METHOD get_tadir_dependency
                  EXPORTING object = 'INTF' obj_name = ls_badi-inter_name
                  RECEIVING dependency = ls_dependency.
                APPEND ls_dependency TO dependencies.
              CATCH cx_root.
                " optional dependency may not exist in the target system
            ENDTRY.
          ENDIF.

          IF ls_badi-flt_type IS NOT INITIAL.
            TRY.
                CLEAR ls_dependency.
                CALL METHOD get_tadir_dependency
                  EXPORTING object = 'DTEL' obj_name = ls_badi-flt_type
                  RECEIVING dependency = ls_dependency.
                APPEND ls_dependency TO dependencies.
              CATCH cx_root.
                " optional dependency may not exist in the target system
            ENDTRY.
          ENDIF.

          IF ls_badi-def_clname IS NOT INITIAL.
            TRY.
                CLEAR ls_dependency.
                CALL METHOD get_tadir_dependency
                  EXPORTING object = 'CLAS' obj_name = ls_badi-def_clname
                  RECEIVING dependency = ls_dependency.
                APPEND ls_dependency TO dependencies.
              CATCH cx_root.
                " optional dependency may not exist in the target system
            ENDTRY.
          ENDIF.

          IF ls_badi-exm_clname IS NOT INITIAL.
            TRY.
                CLEAR ls_dependency.
                CALL METHOD get_tadir_dependency
                  EXPORTING object = 'CLAS' obj_name = ls_badi-exm_clname
                  RECEIVING dependency = ls_dependency.
                APPEND ls_dependency TO dependencies.
              CATCH cx_root.
                " optional dependency may not exist in the target system
            ENDTRY.
          ENDIF.

          IF ls_badi-coc_clname IS NOT INITIAL.
            TRY.
                CLEAR ls_dependency.
                CALL METHOD get_tadir_dependency
                  EXPORTING object = 'CLAS' obj_name = ls_badi-coc_clname
                  RECEIVING dependency = ls_dependency.
                APPEND ls_dependency TO dependencies.
              CATCH cx_root.
                " optional dependency may not exist in the target system
            ENDTRY.
          ENDIF.

          IF lv_ext_class IS NOT INITIAL.
            TRY.
                CLEAR ls_dependency.
                CALL METHOD get_tadir_dependency
                  EXPORTING object = 'CLAS' obj_name = lv_ext_class
                  RECEIVING dependency = ls_dependency.
                APPEND ls_dependency TO dependencies.
              CATCH cx_root.
                " optional dependency may not exist in the target system
            ENDTRY.
          ENDIF.
        ENDIF.
      CATCH cx_root.
        " optional classic BAdI API may not exist in the target system
    ENDTRY.

    lv_name = me->key-obj_name.
    REPLACE ALL OCCURRENCES OF `'` IN lv_name WITH `''`.
    CONCATENATE `EXIT_NAME = '` lv_name `'` INTO lv_where.

    " Structure filter type
    append_table_dependencies(
      EXPORTING table_name   = 'SXS_ATTR'
                where_clause = lv_where
                object_field = 'FLT_TYPE'
                object_type  = 'TABL'
      CHANGING  dependencies = dependencies ).

    " Migration enhancement spot
    append_table_dependencies(
      EXPORTING table_name   = 'SXS_ATTR'
                where_clause = lv_where
                object_field = 'MIG_ENHSPOTNAME'
                object_type  = 'ENHS'
      CHANGING  dependencies = dependencies ).

    " Menu enhancement program
    append_table_dependencies(
      EXPORTING table_name   = 'SXS_FCODE'
                where_clause = lv_where
                object_field = 'GUI_PROG'
                object_type  = 'PROG'
      CHANGING  dependencies = dependencies ).

    " Screen enhancement calling program
    append_table_dependencies(
      EXPORTING table_name   = 'SXS_SCRN'
                where_clause = lv_where
                object_field = 'SCR_A_PROG'
                object_type  = 'PROG'
      CHANGING  dependencies = dependencies ).

    SORT dependencies BY tabname tabkey.
    DELETE ADJACENT DUPLICATES FROM dependencies COMPARING tabname tabkey.
  ENDMETHOD.

ENDCLASS.
