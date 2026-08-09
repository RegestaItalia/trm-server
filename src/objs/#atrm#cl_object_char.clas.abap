CLASS /atrm/cl_object_char DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_char IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    TYPES:
      BEGIN OF lty_field,
        component TYPE fieldname,
        object    TYPE trobjtype,
      END OF lty_field.

    DATA:
      ls_attribute  TYPE cls_attribute,
      lt_fields     TYPE STANDARD TABLE OF lty_field,
      ls_field      TYPE lty_field,
      ls_dependency TYPE /atrm/object_dependency.

    FIELD-SYMBOLS:
      <lv_obj_name> TYPE any.

    TRY.
        SELECT SINGLE *
          FROM cls_attribute
          INTO ls_attribute
          WHERE name = me->key-obj_name
            AND activation_state = 'A'.

        CHECK sy-subrc = 0.

        ls_field-component = 'VALUE_TABLE'.
        ls_field-object = 'TABL'.
        APPEND ls_field TO lt_fields.
        ls_field-component = 'VTEXT_TABLE'.
        ls_field-object = 'TABL'.
        APPEND ls_field TO lt_fields.
        ls_field-component = 'TYPE_GROUP'.
        ls_field-object = 'OTGR'.
        APPEND ls_field TO lt_fields.
        ls_field-component = 'OBJS_OF_TYPEGR'.
        ls_field-object = 'OTGR'.
        APPEND ls_field TO lt_fields.
        ls_field-component = 'IS_ASPECT_FOR'.
        ls_field-object = 'CHAR'.
        APPEND ls_field TO lt_fields.

        LOOP AT lt_fields INTO ls_field.
          ASSIGN COMPONENT ls_field-component OF STRUCTURE ls_attribute
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
        " optional characteristic API may not exist in the target system
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
