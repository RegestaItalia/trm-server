CLASS /atrm/cl_object_devc DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_devc IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    TYPES:
      BEGIN OF lty_field,
        component TYPE fieldname,
        object    TYPE trobjtype,
      END OF lty_field,
      ltyt_field       TYPE STANDARD TABLE OF lty_field WITH DEFAULT KEY,
      ltyt_permission  TYPE STANDARD TABLE OF permission WITH DEFAULT KEY,
      ltyt_sfw_package TYPE STANDARD TABLE OF sfw_package WITH DEFAULT KEY.

    DATA:
      ls_tdevc       TYPE tdevc,
      lt_fields      TYPE ltyt_field,
      ls_field       TYPE lty_field,
      lt_permissions TYPE ltyt_permission,
      ls_permission  TYPE permission,
      lt_sfw_package TYPE ltyt_sfw_package,
      ls_sfw_package TYPE sfw_package,
      ls_dependency  TYPE /atrm/object_dependency.

    FIELD-SYMBOLS:
      <lv_obj_name> TYPE any.

    TRY.
        SELECT SINGLE *
          FROM tdevc
          INTO ls_tdevc
          WHERE devclass = me->key-obj_name.

        CHECK sy-subrc = 0.

        ls_field-component = 'PARENTCL'.
        ls_field-object = 'DEVC'.
        APPEND ls_field TO lt_fields.
        ls_field-component = 'MAINPACK'.
        ls_field-object = 'DEVC'.
        APPEND ls_field TO lt_fields.
        ls_field-component = 'ENHANCED_PACKAGE'.
        ls_field-object = 'DEVC'.
        APPEND ls_field TO lt_fields.
        ls_field-component = 'COMPONENT'.
        ls_field-object = 'BMFR'.
        APPEND ls_field TO lt_fields.
        ls_field-component = 'DEFAULT_INTF'.
        ls_field-object = 'PINF'.
        APPEND ls_field TO lt_fields.
        ls_field-component = 'SWITCH_ID'.
        ls_field-object = 'SFSW'.
        APPEND ls_field TO lt_fields.

        LOOP AT lt_fields INTO ls_field.
          ASSIGN COMPONENT ls_field-component OF STRUCTURE ls_tdevc
            TO <lv_obj_name>.
          CHECK sy-subrc = 0.
          CHECK <lv_obj_name> IS NOT INITIAL.

          TRY.
              CLEAR ls_dependency.
              CALL METHOD get_tadir_dependency
                EXPORTING
                  object     = ls_field-object
                  obj_name   = <lv_obj_name>
                RECEIVING
                  dependency = ls_dependency.

              APPEND ls_dependency TO dependencies.
            CATCH cx_root.
              " optional dependency may not exist in the target system
          ENDTRY.
        ENDLOOP.

        SELECT *
          FROM permission
          INTO TABLE lt_permissions
          WHERE client_pak = me->key-obj_name.

        LOOP AT lt_permissions INTO ls_permission.
          TRY.
              CLEAR ls_dependency.
              CALL METHOD get_tadir_dependency
                EXPORTING
                  object     = 'PINF'
                  obj_name   = ls_permission-intf_name
                RECEIVING
                  dependency = ls_dependency.

              APPEND ls_dependency TO dependencies.
            CATCH cx_root.
              " referenced package interface may no longer exist
          ENDTRY.
        ENDLOOP.

        SELECT *
          FROM sfw_package
          INTO TABLE lt_sfw_package
          WHERE devclass = me->key-obj_name
            AND version = 'A'.

        LOOP AT lt_sfw_package INTO ls_sfw_package.
          TRY.
              CLEAR ls_dependency.
              CALL METHOD get_tadir_dependency
                EXPORTING
                  object     = 'SFSW'
                  obj_name   = ls_sfw_package-switch_id
                RECEIVING
                  dependency = ls_dependency.

              APPEND ls_dependency TO dependencies.
            CATCH cx_root.
              " referenced switch may no longer exist
          ENDTRY.
        ENDLOOP.
      CATCH cx_root.
        " optional package API may not exist in the target system
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
