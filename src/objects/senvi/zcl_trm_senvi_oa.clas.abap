CLASS zcl_trm_senvi_oa DEFINITION
  PUBLIC
  INHERITING FROM zcl_trm_senvi_map
  CREATE PUBLIC.

  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS determine REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_trm_senvi_oa IMPLEMENTATION.

  METHOD determine.
    " could be either a class or an interface
    TRY.
        APPEND zcl_trm_object=>get_tadir_dependency(
          object   = 'CLAS'
          obj_name = sanitize_object_name( raw_name = senvi-encl_obj max_length = 30 )
        ) TO dependencies.
      CATCH zcx_trm_exception.
        APPEND zcl_trm_object=>get_tadir_dependency(
          object   = 'INTF'
          obj_name = sanitize_object_name( raw_name = senvi-encl_obj max_length = 30 )
        ) TO dependencies.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
