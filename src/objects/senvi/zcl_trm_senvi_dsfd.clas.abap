CLASS zcl_trm_senvi_dsfd DEFINITION
  PUBLIC
  INHERITING FROM zcl_trm_senvi_map
  CREATE PUBLIC.

  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS determine REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_trm_senvi_dsfd IMPLEMENTATION.

  METHOD determine.
    APPEND zcl_trm_object=>get_tadir_dependency(
      object   = 'DSFD'
      obj_name = sanitize_object_name( raw_name = senvi-object max_length = 26 )
    ) TO dependencies.
  ENDMETHOD.

ENDCLASS.
