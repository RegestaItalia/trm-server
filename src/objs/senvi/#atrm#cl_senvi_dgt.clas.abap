CLASS /atrm/cl_senvi_dgt DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_senvi_map
  CREATE PUBLIC.

  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS determine REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_senvi_dgt IMPLEMENTATION.

  METHOD determine.
    APPEND /atrm/cl_object=>get_tadir_dependency(
      object   = 'TYPE'
      obj_name = sanitize_object_name( raw_name = senvi-encl_obj max_length = 5 )
    ) TO dependencies.
  ENDMETHOD.

ENDCLASS.
