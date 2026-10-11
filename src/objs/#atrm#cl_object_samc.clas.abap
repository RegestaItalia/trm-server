CLASS /atrm/cl_object_samc DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_samc IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      lt_authorities TYPE STANDARD TABLE OF amc_chnl_auth,
      ls_authority   TYPE amc_chnl_auth,
      lv_class_flag  TYPE c,
      lv_class_name  TYPE seoclsname,
      lv_fugr_flag   TYPE c,
      lv_fugr_name   TYPE rs38l-area,
      lv_object      TYPE trobjtype,
      lv_obj_name    TYPE sobj_name,
      ls_dependency  TYPE /atrm/object_dependency.

    TRY.
        SELECT *
          FROM amc_chnl_auth
          INTO TABLE lt_authorities
          WHERE application_id = me->key-obj_name
            AND version = 'A'.

        LOOP AT lt_authorities INTO ls_authority.
          CHECK ls_authority-program_id IS NOT INITIAL.

          CLEAR:
            lv_class_flag,
            lv_class_name,
            lv_fugr_flag,
            lv_fugr_name,
            lv_object,
            lv_obj_name.

          CALL FUNCTION 'RS_PROGNAME_SPLIT'
            EXPORTING
              progname_with_namespace = ls_authority-program_id
            IMPORTING
              fugr_is_name            = lv_fugr_flag
              fugr_group              = lv_fugr_name
              class_is_name           = lv_class_flag
              class_name              = lv_class_name
            EXCEPTIONS
              delimiter_error = 1
              OTHERS          = 2.

          IF sy-subrc = 0 AND lv_class_flag IS NOT INITIAL.
            lv_object = 'CLAS'.
            lv_obj_name = lv_class_name.
          ELSEIF sy-subrc = 0 AND lv_fugr_flag IS NOT INITIAL.
            lv_object = 'FUGR'.
            lv_obj_name = lv_fugr_name.
          ELSE.
            lv_object = 'PROG'.
            lv_obj_name = ls_authority-program_id.
          ENDIF.

          TRY.
              CLEAR ls_dependency.
              CALL METHOD get_tadir_dependency
                EXPORTING
                  object     = lv_object
                  obj_name   = lv_obj_name
                RECEIVING
                  dependency = ls_dependency.

              APPEND ls_dependency TO dependencies.
            CATCH cx_root.
              " optional dependency may not exist in the target system
          ENDTRY.
        ENDLOOP.
      CATCH cx_root.
        " optional messaging channel API may not exist in the target system
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
