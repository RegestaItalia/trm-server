CLASS zcl_trm_senvi_wdya DEFINITION
  PUBLIC
  INHERITING FROM zcl_trm_senvi_map
  CREATE PUBLIC.

  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS determine REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_trm_senvi_wdya IMPLEMENTATION.

  METHOD determine.
    APPEND zcl_trm_object=>get_tadir_dependency(
      object   = 'WDYA'
      obj_name = sanitize_object_name( raw_name = senvi-object max_length = 25 )
    ) TO dependencies.
  ENDMETHOD.

ENDCLASS.
