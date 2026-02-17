CLASS zcl_trm_object_stc1 DEFINITION
  PUBLIC
  INHERITING FROM zcl_trm_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS zif_trm_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_trm_object_stc1 IMPLEMENTATION.

  METHOD zif_trm_object~get_dependencies.
    " empty implementation
  ENDMETHOD.

ENDCLASS.
