CLASS /atrm/cl_object_clas DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_abap
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING key TYPE /atrm/object.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_clas IMPLEMENTATION.

  METHOD constructor.
    super->constructor(
      object = 'CLAS'
      key    = key
    ).
  ENDMETHOD.

ENDCLASS.
