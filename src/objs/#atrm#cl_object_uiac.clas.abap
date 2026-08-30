CLASS /atrm/cl_object_uiac DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_uiac IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    " object cannot have dependencies
  ENDMETHOD.

ENDCLASS.
