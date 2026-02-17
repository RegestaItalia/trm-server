CLASS zcl_trm_object_fugr DEFINITION
  PUBLIC
  INHERITING FROM zcl_trm_abap
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING key TYPE ztrm_object.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_trm_object_fugr IMPLEMENTATION.

  METHOD constructor.
    super->constructor(
      object = 'FUGR'
      key    = key
    ).
  ENDMETHOD.

ENDCLASS.
