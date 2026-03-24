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
    " could be either a class or an interface
    TRY.
        APPEND /atrm/cl_object=>get_tadir_dependency(
          object   = 'CLAS'
          obj_name = sanitize_object_name( raw_name = senvi-encl_obj max_length = 30 )
        ) TO dependencies.
      CATCH /atrm/cx_exception.
        APPEND /atrm/cl_object=>get_tadir_dependency(
          object   = 'INTF'
          obj_name = sanitize_object_name( raw_name = senvi-encl_obj max_length = 30 )
        ) TO dependencies.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
