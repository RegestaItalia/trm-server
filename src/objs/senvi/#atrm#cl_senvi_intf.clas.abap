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
    DATA: obj_name TYPE string.
    obj_name = senvi-object.
    REPLACE ALL OCCURRENCES OF REGEX '~.*$' IN obj_name WITH space.
    APPEND /atrm/cl_object=>get_tadir_dependency(
      object   = 'INTF'
      obj_name = sanitize_object_name( raw_name = obj_name max_length = 30 )
    ) TO dependencies.
  ENDMETHOD.

ENDCLASS.
