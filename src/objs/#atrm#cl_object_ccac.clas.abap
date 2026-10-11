CLASS /atrm/cl_object_ccac DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_ccac IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    CALL METHOD append_lrep_dependencies
      EXPORTING
        object_type = 'CCAC'
        object_name = me->key-obj_name
      CHANGING
        dependencies = dependencies.
  ENDMETHOD.

ENDCLASS.
