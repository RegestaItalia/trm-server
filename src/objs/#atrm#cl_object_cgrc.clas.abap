CLASS /atrm/cl_object_cgrc DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_cgrc IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    CALL METHOD append_lrep_dependencies
      EXPORTING
        object_type = 'CGRC'
        object_name = me->key-obj_name
      CHANGING
        dependencies = dependencies.
  ENDMETHOD.

ENDCLASS.
