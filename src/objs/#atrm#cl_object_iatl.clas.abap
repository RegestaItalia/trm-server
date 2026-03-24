CLASS /atrm/cl_object_iatl DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_iatl IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    " empty implementation
  ENDMETHOD.

ENDCLASS.
