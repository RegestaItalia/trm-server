CLASS /atrm/cl_object_srvb DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_srvb IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      ls_object_type TYPE wbobjtype,
      lo_operator    TYPE REF TO object,
      lo_data_model  TYPE REF TO object,
      lr_data        TYPE REF TO data,
      lv_data_type   TYPE string,
      ls_dependency  TYPE /atrm/object_dependency.

    FIELD-SYMBOLS:
      <ls_data>       TYPE any,
      <ls_content>    TYPE any,
      <lt_services>   TYPE ANY TABLE,
      <ls_service>    TYPE any,
      <lt_versions>   TYPE ANY TABLE,
      <ls_version>    TYPE any,
      <lv_srvd_name>  TYPE any.

    TRY.
        ls_object_type-objtype_tr = 'SRVB'.
        ls_object_type-subtype_wb = 'SVB'.

        CALL METHOD ('CL_WB_OBJECT_OPERATOR')=>('CREATE_INSTANCE')
          EXPORTING object_type = ls_object_type object_key = me->key-obj_name
          RECEIVING result = lo_operator.
        CALL METHOD lo_operator->('IF_WB_OBJECT_OPERATOR~READ')
          EXPORTING version = 'A' data_selection = 'AL'
          IMPORTING eo_object_data = lo_data_model.
        CALL METHOD lo_data_model->('IF_WB_OBJECT_DATA_MODEL~GET_DATATYPE_NAME')
          EXPORTING p_data_selection = 'AL'
          RECEIVING result = lv_data_type.
        CHECK lv_data_type IS NOT INITIAL.

        CREATE DATA lr_data TYPE (lv_data_type).
        ASSIGN lr_data->* TO <ls_data>.
        CALL METHOD lo_data_model->('IF_WB_OBJECT_DATA_MODEL~GET_SELECTED_DATA')
          EXPORTING p_data_selection = 'AL'
          IMPORTING p_data = <ls_data>.

        ASSIGN COMPONENT 'CONTENT' OF STRUCTURE <ls_data> TO <ls_content>.
        IF sy-subrc <> 0.
          ASSIGN <ls_data> TO <ls_content>.
        ENDIF.

        ASSIGN COMPONENT 'SERVICES' OF STRUCTURE <ls_content>
          TO <lt_services>.
        IF sy-subrc = 0.
          LOOP AT <lt_services> ASSIGNING <ls_service>.
            ASSIGN COMPONENT 'SERVICE_CONTENT' OF STRUCTURE <ls_service>
              TO <lt_versions>.
            CHECK sy-subrc = 0.

            LOOP AT <lt_versions> ASSIGNING <ls_version>.
              ASSIGN COMPONENT 'SRVD_REF-NAME' OF STRUCTURE <ls_version>
                TO <lv_srvd_name>.
              CHECK sy-subrc = 0.
              CHECK <lv_srvd_name> IS NOT INITIAL.

              TRY.
                  CLEAR ls_dependency.
                  CALL METHOD get_tadir_dependency
                    EXPORTING object = 'SRVD' obj_name = <lv_srvd_name>
                    RECEIVING dependency = ls_dependency.
                  APPEND ls_dependency TO dependencies.
                CATCH cx_root.
                  " optional dependency may not exist in the target system
              ENDTRY.
            ENDLOOP.
          ENDLOOP.
        ENDIF.
      CATCH cx_root.
        " optional service binding API may not exist in the target system
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
