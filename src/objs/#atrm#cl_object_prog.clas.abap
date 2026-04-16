CLASS /atrm/cl_object_prog DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_abap
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING key TYPE /atrm/object.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_prog IMPLEMENTATION.

  METHOD constructor.
    super->constructor(
      object = 'PROG'
      key    = key
    ).
  ENDMETHOD.

ENDCLASS.
