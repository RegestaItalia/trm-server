CLASS /atrm/cl_object_xinx DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_xinx IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      lv_length     TYPE i,
      lv_offset     TYPE i,
      lv_tabname    TYPE ddobjname,
      ls_dependency TYPE /atrm/object_dependency.

    TRY.
        lv_length = strlen( me->key-obj_name ).
        CHECK lv_length > 3.

        lv_offset = lv_length - 3.
        lv_tabname = me->key-obj_name(lv_offset).
        CONDENSE lv_tabname.
        CHECK lv_tabname IS NOT INITIAL.

        CALL METHOD get_tadir_dependency
          EXPORTING
            object     = 'TABL'
            obj_name   = lv_tabname
          RECEIVING
            dependency = ls_dependency.

        APPEND ls_dependency TO dependencies.
      CATCH cx_root.
        " optional dependency may not exist in the target system
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
