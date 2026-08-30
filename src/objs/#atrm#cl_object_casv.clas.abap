CLASS /atrm/cl_object_casv DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_casv IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      lv_aspect      TYPE sobj_name,
      ls_dependency TYPE /atrm/object_dependency.

    TRY.
        lv_aspect = me->key-obj_name(30).
        IF lv_aspect IS NOT INITIAL.
          CALL METHOD get_tadir_dependency
            EXPORTING object = 'CASP' obj_name = lv_aspect
            RECEIVING dependency = ls_dependency.
          APPEND ls_dependency TO dependencies.
        ENDIF.
      CATCH cx_root.
        " optional parent aspect may not exist in the target system
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
