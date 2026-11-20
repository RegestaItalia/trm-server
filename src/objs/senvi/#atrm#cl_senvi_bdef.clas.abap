CLASS /atrm/cl_senvi_bdef DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_senvi_map
  CREATE PUBLIC.

  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS determine REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_senvi_bdef IMPLEMENTATION.

  METHOD determine.
    DATA ls_dependency TYPE /atrm/object_dependency.
    DATA lv_obj_name TYPE string.
    " can have multiple behaviour definitions in one BDEF object, so tadir entry might not exist
    lv_obj_name = sanitize_object_name( raw_name = senvi-object max_length = 30 ).
    ls_dependency = /atrm/cl_object=>get_tadir_dependency(
      object   = 'BDEF'
      obj_name = lv_obj_name
    ).
    APPEND ls_dependency TO dependencies.
  ENDMETHOD.

ENDCLASS.
