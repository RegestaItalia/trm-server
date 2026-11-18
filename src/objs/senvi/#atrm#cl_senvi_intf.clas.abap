CLASS /atrm/cl_senvi_intf DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_senvi_map
  CREATE PUBLIC.

  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS determine REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_senvi_intf IMPLEMENTATION.

  METHOD determine.
    DATA ls_dependency TYPE /atrm/object_dependency.
    DATA lv_obj_name TYPE string.
    DATA: obj_name TYPE string.
    obj_name = senvi-object.
    REPLACE ALL OCCURRENCES OF REGEX '~.*$' IN obj_name WITH space.
    lv_obj_name = sanitize_object_name( raw_name = obj_name max_length = 30 ).
    ls_dependency = /atrm/cl_object=>get_tadir_dependency(
      object   = 'INTF'
      obj_name = lv_obj_name
    ).
    APPEND ls_dependency TO dependencies.
  ENDMETHOD.

ENDCLASS.
