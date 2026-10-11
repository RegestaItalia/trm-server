CLASS /atrm/cl_object_omlm DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_omlm IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    " object cannot have dependencies
  ENDMETHOD.

ENDCLASS.
