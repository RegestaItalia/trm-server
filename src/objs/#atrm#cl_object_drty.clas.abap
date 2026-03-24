CLASS /atrm/cl_object_drty DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_drty IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    " empty implementation
  ENDMETHOD.

ENDCLASS.
