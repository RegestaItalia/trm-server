CLASS /atrm/cl_object_jobd DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_jobd IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      lv_name        TYPE c LENGTH 32,
      lr_definition  TYPE REF TO data,
      lo_definition  TYPE REF TO object,
      ls_dependency  TYPE /atrm/object_dependency.

    FIELD-SYMBOLS:
      <ls_definition> TYPE any,
      <lv_class_name> TYPE any.

    TRY.
        lv_name = me->key-obj_name.

        CREATE DATA lr_definition
          TYPE ('CL_JR_JOB_DEFINITION=>TY_JOB_DEFINITION').
        ASSIGN lr_definition->* TO <ls_definition>.

        CREATE OBJECT lo_definition TYPE ('CL_JR_JOB_DEFINITION')
          EXPORTING
            im_jd_name = lv_name.

        CALL METHOD lo_definition->('GET_JD_ATTRIBUTES')
          IMPORTING
            ex_jd_attributes = <ls_definition>.

        ASSIGN COMPONENT 'SCOPE_CLASS' OF STRUCTURE <ls_definition>
          TO <lv_class_name>.
        CHECK sy-subrc = 0.
        CHECK <lv_class_name> IS NOT INITIAL.

        CALL METHOD get_tadir_dependency
          EXPORTING
            object     = 'CLAS'
            obj_name   = <lv_class_name>
          RECEIVING
            dependency = ls_dependency.

        APPEND ls_dependency TO dependencies.
      CATCH cx_root.
        " optional job definition API may not exist in the target system
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
