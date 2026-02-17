CLASS zcl_trm_senvi_view DEFINITION
  PUBLIC
  INHERITING FROM zcl_trm_senvi_map
  CREATE PUBLIC.

  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS determine REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_trm_senvi_view IMPLEMENTATION.

  METHOD determine.
    " could be either a view or a cds
    TRY.
        APPEND zcl_trm_object=>get_tadir_dependency(
          object   = 'VIEW'
          obj_name = sanitize_object_name( raw_name = senvi-object max_length = 16 )
        ) TO dependencies.
      CATCH zcx_trm_exception.
        APPEND zcl_trm_object=>get_tadir_dependency(
          object   = 'DDLS'
          obj_name = sanitize_object_name( raw_name = senvi-object max_length = 30 )
        ) TO dependencies.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
