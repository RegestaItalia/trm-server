CLASS /atrm/cl_object_aobj DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_aobj IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      ls_arch_obj    TYPE arch_obj,
      lt_arch_def    TYPE STANDARD TABLE OF arch_def,
      ls_arch_def    TYPE arch_def,
      lt_arch_oclas  TYPE STANDARD TABLE OF arch_oclas,
      ls_arch_oclas  TYPE arch_oclas,
      lt_arch_class  TYPE STANDARD TABLE OF arch_class,
      ls_arch_class  TYPE arch_class,
      lt_fields      TYPE STANDARD TABLE OF fieldname,
      lv_field       TYPE fieldname,
      ls_dependency  TYPE /atrm/object_dependency.

    FIELD-SYMBOLS:
      <lv_obj_name> TYPE any.

    TRY.
        SELECT SINGLE *
          FROM arch_obj
          INTO ls_arch_obj
          WHERE object = me->key-obj_name.

        IF sy-subrc = 0.
          APPEND 'REORGA_PRG' TO lt_fields.
          APPEND 'RETRIE_PRG' TO lt_fields.
          APPEND 'DELETE_PRG' TO lt_fields.
          APPEND 'ARCH_CONV' TO lt_fields.
          APPEND 'ARCH_XPRA' TO lt_fields.
          APPEND 'EXIT_PROG' TO lt_fields.
          APPEND 'FIRST_PRG' TO lt_fields.
          APPEND 'LAST_PRG' TO lt_fields.
          APPEND 'IDXBUI_PRG' TO lt_fields.
          APPEND 'IDXDEL_PRG' TO lt_fields.

          LOOP AT lt_fields INTO lv_field.
            ASSIGN COMPONENT lv_field OF STRUCTURE ls_arch_obj
              TO <lv_obj_name>.
            CHECK sy-subrc = 0.
            CHECK <lv_obj_name> IS NOT INITIAL.

            TRY.
                CLEAR ls_dependency.
                CALL METHOD get_tadir_dependency
                  EXPORTING
                    object     = 'PROG'
                    obj_name   = <lv_obj_name>
                  RECEIVING
                    dependency = ls_dependency.

                APPEND ls_dependency TO dependencies.
              CATCH cx_root.
                " optional dependency may not exist in the target system
            ENDTRY.
          ENDLOOP.

          CLEAR lt_fields.
          APPEND 'STORAGE_CLASS' TO lt_fields.
          APPEND 'WRITE_CLASS' TO lt_fields.
          APPEND 'DELETE_CLASS' TO lt_fields.
          APPEND 'RELOAD_CLASS' TO lt_fields.

          LOOP AT lt_fields INTO lv_field.
            ASSIGN COMPONENT lv_field OF STRUCTURE ls_arch_obj
              TO <lv_obj_name>.
            CHECK sy-subrc = 0.
            CHECK <lv_obj_name> IS NOT INITIAL.

            TRY.
                CLEAR ls_dependency.
                CALL METHOD get_tadir_dependency
                  EXPORTING
                    object     = 'CLAS'
                    obj_name   = <lv_obj_name>
                  RECEIVING
                    dependency = ls_dependency.

                APPEND ls_dependency TO dependencies.
              CATCH cx_root.
                " optional dependency may not exist in the target system
            ENDTRY.
          ENDLOOP.

          ASSIGN COMPONENT 'UFFCTR' OF STRUCTURE ls_arch_obj
            TO <lv_obj_name>.
          IF sy-subrc = 0 AND <lv_obj_name> IS NOT INITIAL.
            TRY.
                CLEAR ls_dependency.
                CALL METHOD get_tadir_dependency
                  EXPORTING
                    object     = 'BMFR'
                    obj_name   = <lv_obj_name>
                  RECEIVING
                    dependency = ls_dependency.

                APPEND ls_dependency TO dependencies.
              CATCH cx_root.
                " optional dependency may not exist in the target system
            ENDTRY.
          ENDIF.
        ENDIF.

        SELECT *
          FROM arch_def
          INTO TABLE lt_arch_def
          WHERE object = me->key-obj_name.

        LOOP AT lt_arch_def INTO ls_arch_def.
          CHECK ls_arch_def-structure IS NOT INITIAL.

          TRY.
              CLEAR ls_dependency.
              CALL METHOD get_tadir_dependency
                EXPORTING
                  object     = 'TABL'
                  obj_name   = ls_arch_def-structure
                RECEIVING
                  dependency = ls_dependency.

              APPEND ls_dependency TO dependencies.
            CATCH cx_root.
              " optional dependency may not exist in the target system
          ENDTRY.
        ENDLOOP.

        SELECT *
          FROM arch_oclas
          INTO TABLE lt_arch_oclas
          WHERE object = me->key-obj_name.

        IF lt_arch_oclas IS NOT INITIAL.
          SELECT *
            FROM arch_class
            INTO TABLE lt_arch_class
            FOR ALL ENTRIES IN lt_arch_oclas
            WHERE arch_class = lt_arch_oclas-arch_class.
        ENDIF.

        LOOP AT lt_arch_class INTO ls_arch_class.
          ASSIGN COMPONENT 'CLASS_FUGR' OF STRUCTURE ls_arch_class
            TO <lv_obj_name>.
          IF sy-subrc = 0 AND <lv_obj_name> IS NOT INITIAL.
            TRY.
                CLEAR ls_dependency.
                CALL METHOD get_tadir_dependency
                  EXPORTING
                    object     = 'FUGR'
                    obj_name   = <lv_obj_name>
                  RECEIVING
                    dependency = ls_dependency.

                APPEND ls_dependency TO dependencies.
              CATCH cx_root.
                " optional dependency may not exist in the target system
            ENDTRY.
          ENDIF.

          ASSIGN COMPONENT 'ABAP_CLASS' OF STRUCTURE ls_arch_class
            TO <lv_obj_name>.
          IF sy-subrc = 0 AND <lv_obj_name> IS NOT INITIAL.
            TRY.
                CLEAR ls_dependency.
                CALL METHOD get_tadir_dependency
                  EXPORTING
                    object     = 'CLAS'
                    obj_name   = <lv_obj_name>
                  RECEIVING
                    dependency = ls_dependency.

                APPEND ls_dependency TO dependencies.
              CATCH cx_root.
                " optional dependency may not exist in the target system
            ENDTRY.
          ENDIF.
        ENDLOOP.
      CATCH cx_root.
        " optional archiving API may not exist in the target system
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
