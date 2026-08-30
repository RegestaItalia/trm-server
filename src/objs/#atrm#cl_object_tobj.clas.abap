CLASS /atrm/cl_object_tobj DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_tobj IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      lv_table_name TYPE tabname,
      ls_dependency TYPE /atrm/object_dependency.

    TRY.
        lv_table_name = me->key-obj_name(10).
        CONDENSE lv_table_name.
        CHECK lv_table_name IS NOT INITIAL.

        CALL METHOD get_tadir_dependency
          EXPORTING object = 'TABL' obj_name = lv_table_name
          RECEIVING dependency = ls_dependency.
        APPEND ls_dependency TO dependencies.
      CATCH cx_root.
        " optional dependency may not exist in the target system
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
