CLASS /atrm/cl_object_sfpi DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_sfpi IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    " empty implementation
  ENDMETHOD.

ENDCLASS.
