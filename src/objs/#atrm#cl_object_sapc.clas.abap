CLASS /atrm/cl_object_sapc DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_sapc IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      lv_class_name TYPE apc_appl-class_name,
      ls_dependency TYPE /atrm/object_dependency.

    TRY.
        SELECT SINGLE class_name
          FROM apc_appl
          INTO lv_class_name
          WHERE application_id = me->key-obj_name
            AND version = 'A'.

        CHECK sy-subrc = 0.
        CHECK lv_class_name IS NOT INITIAL.

        CALL METHOD get_tadir_dependency
          EXPORTING
            object     = 'CLAS'
            obj_name   = lv_class_name
          RECEIVING
            dependency = ls_dependency.

        APPEND ls_dependency TO dependencies.
      CATCH cx_root.
        " optional dependency may not exist in the target system
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
