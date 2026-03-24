CLASS /atrm/cl_senvi_iasp DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_senvi_map
  CREATE PUBLIC.

  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS determine REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_senvi_iasp IMPLEMENTATION.

  METHOD determine.
    APPEND /atrm/cl_object=>get_tadir_dependency(
      object   = 'IASP'
      obj_name = sanitize_object_name( raw_name = senvi-object max_length = 40 no_gaps = ' ' no_dots = ' ' )
    ) TO dependencies.
  ENDMETHOD.

ENDCLASS.
