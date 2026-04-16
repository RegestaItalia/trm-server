CLASS /atrm/cl_object_fugr DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_abap
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING key TYPE /atrm/object.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_fugr IMPLEMENTATION.

  METHOD constructor.
    super->constructor(
      object = 'FUGR'
      key    = key
    ).
  ENDMETHOD.

ENDCLASS.
