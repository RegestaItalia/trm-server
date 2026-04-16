CLASS /atrm/cl_senvi_enhs DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_senvi_map
  CREATE PUBLIC.

  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS determine REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_senvi_enhs IMPLEMENTATION.

  METHOD determine.
    APPEND /atrm/cl_object=>get_tadir_dependency(
      object   = 'ENHS'
      obj_name = sanitize_object_name( raw_name = senvi-object max_length = 30 )
    ) TO dependencies.
  ENDMETHOD.

ENDCLASS.
