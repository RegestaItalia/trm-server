CLASS /atrm/cl_senvi_suso DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_senvi_map
  CREATE PUBLIC.

  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS determine REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_senvi_suso IMPLEMENTATION.

  METHOD determine.
    APPEND /atrm/cl_object=>get_tadir_dependency(
      object   = 'SUSO'
      obj_name = sanitize_object_name( raw_name = senvi-object max_length = 10 )
    ) TO dependencies.
  ENDMETHOD.

ENDCLASS.
