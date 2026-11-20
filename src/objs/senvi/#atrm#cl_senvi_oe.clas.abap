CLASS /atrm/cl_senvi_oe DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_senvi_map
  CREATE PUBLIC.

  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS determine REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_senvi_oe IMPLEMENTATION.

  METHOD determine.
    DATA ls_dependency TYPE /atrm/object_dependency.
    DATA lv_obj_name TYPE string.
    " could be either a class or an interface
    TRY.
        lv_obj_name = sanitize_object_name( raw_name = senvi-encl_obj max_length = 30 ).
        ls_dependency = /atrm/cl_object=>get_tadir_dependency(
          object   = 'CLAS'
          obj_name = lv_obj_name
        ).
        APPEND ls_dependency TO dependencies.
      CATCH /atrm/cx_exception.
        lv_obj_name = sanitize_object_name( raw_name = senvi-encl_obj max_length = 30 ).
        ls_dependency = /atrm/cl_object=>get_tadir_dependency(
          object   = 'INTF'
          obj_name = lv_obj_name
        ).
        APPEND ls_dependency TO dependencies.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
