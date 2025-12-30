CLASS zcl_trm_senvi_mess DEFINITION
  PUBLIC
  INHERITING FROM zcl_trm_senvi_map
  CREATE PUBLIC.

  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS determine REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_trm_senvi_mess IMPLEMENTATION.

  METHOD determine.
    APPEND zcl_trm_object=>get_tadir_dependency(
      object   = 'MSAG'
      obj_name = sanitize_object_name( raw_name = senvi-encl_obj max_length = 20 )
    ) TO dependencies.
  ENDMETHOD.

ENDCLASS.
