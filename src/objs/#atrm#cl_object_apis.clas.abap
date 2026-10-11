CLASS /atrm/cl_object_apis DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_apis IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      lv_object     TYPE trobjtype,
      lv_obj_name   TYPE sobj_name,
      ls_dependency TYPE /atrm/object_dependency.

    TRY.
        lv_obj_name = me->key-obj_name(36).
        CONDENSE lv_obj_name.
        lv_object = me->key-obj_name+36(4).

        CHECK lv_object IS NOT INITIAL.
        CHECK lv_obj_name IS NOT INITIAL.

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
  ENDMETHOD.

ENDCLASS.
