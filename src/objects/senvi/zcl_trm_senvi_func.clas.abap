CLASS zcl_trm_senvi_func DEFINITION
  PUBLIC
  INHERITING FROM zcl_trm_senvi_map
  CREATE PUBLIC.

  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS determine REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_trm_senvi_func IMPLEMENTATION.

  METHOD determine.
    DATA: obj_name TYPE string,
          object   TYPE string.
    FIELD-SYMBOLS <fs_tfdir> TYPE ztrm_object_dependency.
    IF senvi-object CP 'ENQUEUE_*' OR senvi-object CP 'DEQUEUE_*'.
      obj_name = senvi-object.
      REPLACE FIRST OCCURRENCE OF REGEX '^\s*(ENQUEUE|DEQUEUE)_' IN obj_name WITH space.
      obj_name = sanitize_object_name( raw_name = obj_name max_length = 16 ).
      object = 'ENQU'.
    ELSE.
      obj_name = sanitize_object_name( raw_name = senvi-encl_obj max_length = 26 ).
      object = 'FUGR'.
    ENDIF.
    APPEND zcl_trm_object=>get_tadir_dependency(
      object   = object
      obj_name = obj_name
    ) TO dependencies.
    IF object EQ 'FUGR'.
      APPEND INITIAL LINE TO dependencies ASSIGNING <fs_tfdir>.
      <fs_tfdir>-tabname = 'TFDIR'.
      <fs_tfdir>-tabkey = sanitize_object_name( raw_name = senvi-object max_length = 30 ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.
