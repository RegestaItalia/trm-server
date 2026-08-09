CLASS /atrm/cl_object_vkoi DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_vkoi IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA ls_dependency TYPE /atrm/object_dependency.
    TRY.
        CALL METHOD get_tadir_dependency
          EXPORTING object = 'TABL' obj_name = me->key-obj_name
          RECEIVING dependency = ls_dependency.
        IF ls_dependency IS NOT INITIAL.
          APPEND ls_dependency TO dependencies.
        ENDIF.
      CATCH cx_root.
        " generated condition table may not exist in the target system
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
