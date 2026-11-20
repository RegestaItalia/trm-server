CLASS /atrm/cl_senvi_view DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_senvi_map
  CREATE PUBLIC.

  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS determine REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_senvi_view IMPLEMENTATION.

  METHOD determine.
    DATA ls_dependency TYPE /atrm/object_dependency.
    DATA lv_obj_name TYPE string.
    " could be either a view or a cds
    TRY.
        lv_obj_name = sanitize_object_name( raw_name = senvi-object max_length = 16 ).
        ls_dependency = /atrm/cl_object=>get_tadir_dependency(
          object   = 'VIEW'
          obj_name = lv_obj_name
        ).
        APPEND ls_dependency TO dependencies.
      CATCH /atrm/cx_exception.
        lv_obj_name = sanitize_object_name( raw_name = senvi-object max_length = 30 ).
        ls_dependency = /atrm/cl_object=>get_tadir_dependency(
          object   = 'DDLS'
          obj_name = lv_obj_name
        ).
        APPEND ls_dependency TO dependencies.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
