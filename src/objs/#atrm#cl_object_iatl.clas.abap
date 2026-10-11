CLASS /atrm/cl_object_iatl DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_iatl IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      lv_service    TYPE sobj_name,
      ls_dependency TYPE /atrm/object_dependency.

    lv_service = me->key-obj_name(14).
    TRY.
        CALL METHOD get_tadir_dependency
          EXPORTING object = 'IASP' obj_name = lv_service
          RECEIVING dependency = ls_dependency.
        APPEND ls_dependency TO dependencies.
      CATCH cx_root.
        " optional parent IAC service may not exist in the target system
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
