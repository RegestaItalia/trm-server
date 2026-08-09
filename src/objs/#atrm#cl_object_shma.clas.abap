CLASS /atrm/cl_object_shma DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_shma IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      lv_root       TYPE shma_attributes-root,
      ls_dependency TYPE /atrm/object_dependency.

    TRY.
        SELECT SINGLE root
          FROM shma_attributes
          INTO lv_root
          WHERE area_name = me->key-obj_name.

        CHECK sy-subrc = 0.
        CHECK lv_root IS NOT INITIAL.

        CALL METHOD get_tadir_dependency
          EXPORTING
            object     = 'CLAS'
            obj_name   = lv_root
          RECEIVING
            dependency = ls_dependency.

        APPEND ls_dependency TO dependencies.
      CATCH cx_root.
        " optional dependency may not exist in the target system
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
