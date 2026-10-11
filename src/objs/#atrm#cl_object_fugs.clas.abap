CLASS /atrm/cl_object_fugs DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_fugs IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      ls_key    TYPE /atrm/object,
      lo_object TYPE REF TO /atrm/cl_object.

    ls_key = me->key.
    ls_key-object = 'PROG'.
    CONCATENATE 'SAPL' me->key-obj_name INTO ls_key-obj_name.

    TRY.
        CREATE OBJECT lo_object
          EXPORTING key = ls_key.
        CALL METHOD lo_object->/atrm/if_object~get_dependencies
          IMPORTING dependencies = dependencies.
      CATCH cx_root.
        " the generated function-pool program may not exist
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
