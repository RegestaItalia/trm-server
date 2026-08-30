CLASS /atrm/cl_object_avar DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_avar IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      lv_name       TYPE aab_var_name,
      lo_variant    TYPE REF TO cl_aab_variant,
      lt_ids        TYPE aab_var_obj_act_tab,
      ls_id         LIKE LINE OF lt_ids,
      ls_dependency TYPE /atrm/object_dependency.

    TRY.
        lv_name = me->key-obj_name.

        CREATE OBJECT lo_variant
          EXPORTING
            im_name  = lv_name
            im_local = space
          EXCEPTIONS
            name_not_allowed = 1
            user_not_valid   = 2
            no_authorization = 3
            OTHERS           = 4.

        IF sy-subrc <> 0.
          RETURN.
        ENDIF.

        CALL METHOD lo_variant->get_ids
          EXPORTING
            im_enforce_db_read = abap_true
          IMPORTING
            ex_ids             = lt_ids
          EXCEPTIONS
            id_not_valid = 1
            OTHERS       = 2.

        IF sy-subrc <> 0.
          RETURN.
        ENDIF.

        LOOP AT lt_ids INTO ls_id.
          CHECK ls_id-object IS NOT INITIAL.
          CHECK ls_id-name IS NOT INITIAL.

          TRY.
              CLEAR ls_dependency.
              CALL METHOD get_tadir_dependency
                EXPORTING
                  object     = ls_id-object
                  obj_name   = ls_id-name
                RECEIVING
                  dependency = ls_dependency.

              APPEND ls_dependency TO dependencies.
            CATCH cx_root.
              " optional dependency may not exist in the target system
          ENDTRY.
        ENDLOOP.
      CATCH cx_root.
        " optional dependency API may not exist in the target system
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
