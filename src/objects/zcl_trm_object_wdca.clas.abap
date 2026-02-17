CLASS zcl_trm_object_wdca DEFINITION
  PUBLIC
  INHERITING FROM zcl_trm_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS zif_trm_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_trm_object_wdca IMPLEMENTATION.

  METHOD zif_trm_object~get_dependencies.
    " empty implementation
  ENDMETHOD.

ENDCLASS.
