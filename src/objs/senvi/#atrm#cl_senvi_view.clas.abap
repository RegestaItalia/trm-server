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
    " could be either a view or a cds
    TRY.
        APPEND /atrm/cl_object=>get_tadir_dependency(
          object   = 'VIEW'
          obj_name = sanitize_object_name( raw_name = senvi-object max_length = 16 )
        ) TO dependencies.
      CATCH /atrm/cx_exception.
        APPEND /atrm/cl_object=>get_tadir_dependency(
          object   = 'DDLS'
          obj_name = sanitize_object_name( raw_name = senvi-object max_length = 30 )
        ) TO dependencies.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
