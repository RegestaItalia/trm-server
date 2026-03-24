CLASS /atrm/cl_object_wgrp DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_wgrp IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    " empty implementation
  ENDMETHOD.

ENDCLASS.
