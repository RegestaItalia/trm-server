CLASS /atrm/cl_senvi_func DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_senvi_map
  CREATE PUBLIC.

  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS determine REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_senvi_func IMPLEMENTATION.

  METHOD determine.
    DATA ls_dependency TYPE /atrm/object_dependency.
    DATA lv_obj_name TYPE string.
    DATA: obj_name TYPE string,
          object   TYPE string.
    FIELD-SYMBOLS <fs_tfdir> TYPE /atrm/object_dependency.
    IF senvi-object CP 'ENQUEUE_*' OR senvi-object CP 'DEQUEUE_*'.
      obj_name = senvi-object.
      REPLACE FIRST OCCURRENCE OF REGEX '^\s*(ENQUEUE|DEQUEUE)_' IN obj_name WITH space.
      obj_name = sanitize_object_name( raw_name = obj_name max_length = 16 ).
      object = 'ENQU'.
    ELSE.
      obj_name = sanitize_object_name( raw_name = senvi-encl_obj max_length = 26 ).
      object = 'FUGR'.
    ENDIF.
    ls_dependency = /atrm/cl_object=>get_tadir_dependency(
      object   = object
      obj_name = obj_name
    ).
    APPEND ls_dependency TO dependencies.
    IF object EQ 'FUGR'.
      lv_obj_name = sanitize_object_name( raw_name = senvi-object max_length = 30 ).
      ls_dependency = /atrm/cl_object=>get_tfdir_dependency(
        funcname = lv_obj_name
      ).
      APPEND ls_dependency TO dependencies.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
