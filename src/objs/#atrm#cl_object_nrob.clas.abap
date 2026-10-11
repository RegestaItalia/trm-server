CLASS /atrm/cl_object_nrob DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_nrob IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    TYPES:
      BEGIN OF lty_field,
        component TYPE fieldname,
        object    TYPE trobjtype,
      END OF lty_field.

    DATA:
      ls_tnro       TYPE tnro,
      lt_fields     TYPE STANDARD TABLE OF lty_field,
      ls_field      TYPE lty_field,
      ls_dependency TYPE /atrm/object_dependency.

    FIELD-SYMBOLS:
      <lv_obj_name> TYPE any.

    TRY.
        SELECT SINGLE *
          FROM tnro
          INTO ls_tnro
          WHERE object = me->key-obj_name.

        CHECK sy-subrc = 0.

        ls_field-component = 'DTELSOBJ'.
        ls_field-object = 'DTEL'.
        APPEND ls_field TO lt_fields.
        ls_field-component = 'NRTAB'.
        ls_field-object = 'TABL'.
        APPEND ls_field TO lt_fields.
        ls_field-component = 'NRELTXTTAB'.
        ls_field-object = 'TABL'.
        APPEND ls_field TO lt_fields.
        ls_field-component = 'DOMLEN'.
        ls_field-object = 'DOMA'.
        APPEND ls_field TO lt_fields.

        LOOP AT lt_fields INTO ls_field.
          ASSIGN COMPONENT ls_field-component OF STRUCTURE ls_tnro
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
      CATCH cx_root.
        " optional number range API may not exist in the target system
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
