CLASS /atrm/cl_object_srfc DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_srfc IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      lo_persist     TYPE REF TO object,
      lo_data_model  TYPE REF TO object,
      lr_data        TYPE REF TO data,
      ls_dependency  TYPE /atrm/object_dependency.

    FIELD-SYMBOLS:
      <ls_data>      TYPE any,
      <lv_obj_name>  TYPE any.

    TRY.
        CREATE DATA lr_data TYPE ('UCONRFCSERV_COMPLETE').
        ASSIGN lr_data->* TO <ls_data>.

        CREATE OBJECT lo_persist TYPE ('CL_UCONRFC_OBJECT_PERSIST').
        CALL METHOD lo_persist->('IF_WB_OBJECT_PERSIST~GET')
          EXPORTING
            p_object_key  = me->key-obj_name
            p_version     = 'A'
          CHANGING
            p_object_data = lo_data_model.
        CALL METHOD lo_data_model->('IF_WB_OBJECT_DATA_MODEL~GET_DATA')
          IMPORTING p_data = <ls_data>.

        ASSIGN COMPONENT 'HEADER-FUNCNAME' OF STRUCTURE <ls_data>
          TO <lv_obj_name>.
        IF sy-subrc = 0 AND <lv_obj_name> IS NOT INITIAL.
          TRY.
              CALL METHOD get_tfdir_dependency
                EXPORTING funcname = <lv_obj_name>
                RECEIVING dependency = ls_dependency.
              APPEND ls_dependency TO dependencies.
            CATCH cx_root.
              " optional dependency may not exist in the target system
          ENDTRY.
        ENDIF.

        UNASSIGN <lv_obj_name>.
        ASSIGN COMPONENT 'HEADER-DEFAULT_PROFILE' OF STRUCTURE <ls_data>
          TO <lv_obj_name>.
        IF sy-subrc = 0 AND <lv_obj_name> IS NOT INITIAL.
          TRY.
              CLEAR ls_dependency.
              CALL METHOD get_tadir_dependency
                EXPORTING object = 'HTTP' obj_name = <lv_obj_name>
                RECEIVING dependency = ls_dependency.
              APPEND ls_dependency TO dependencies.
            CATCH cx_root.
              " optional dependency may not exist in the target system
          ENDTRY.
        ENDIF.
      CATCH cx_root.
        " optional RFC service API may not exist in the target system
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
