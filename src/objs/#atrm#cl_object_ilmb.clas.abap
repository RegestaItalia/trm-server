CLASS /atrm/cl_object_ilmb DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_ilmb IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      ls_object_type TYPE wbobjtype,
      lo_operator    TYPE REF TO object,
      lo_data_model  TYPE REF TO object,
      lr_data        TYPE REF TO data,
      lv_data_type   TYPE string,
      ls_dependency  TYPE /atrm/object_dependency.

    FIELD-SYMBOLS:
      <ls_data>      TYPE any,
      <ls_content>   TYPE any,
      <lt_items>     TYPE ANY TABLE,
      <ls_item>      TYPE any,
      <lv_obj_name>  TYPE any.

    TRY.
        ls_object_type-objtype_tr = 'ILMB'.
        ls_object_type-subtype_wb = 'IRM'.

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

        ASSIGN COMPONENT 'DESTRUCTION_OBJECT-NAME' OF STRUCTURE <ls_content>
          TO <lv_obj_name>.
        IF sy-subrc = 0 AND <lv_obj_name> IS NOT INITIAL.
          TRY.
              CALL METHOD get_tadir_dependency
                EXPORTING object = 'DOBJ' obj_name = <lv_obj_name>
                RECEIVING dependency = ls_dependency.
              APPEND ls_dependency TO dependencies.
            CATCH cx_root.
              " optional dependency may not exist in the target system
          ENDTRY.
        ENDIF.

        UNASSIGN <lv_obj_name>.
        ASSIGN COMPONENT 'CALLBACK_CLASS-NAME' OF STRUCTURE <ls_content>
          TO <lv_obj_name>.
        IF sy-subrc = 0 AND <lv_obj_name> IS NOT INITIAL.
          TRY.
              CLEAR ls_dependency.
              CALL METHOD get_tadir_dependency
                EXPORTING object = 'CLAS' obj_name = <lv_obj_name>
                RECEIVING dependency = ls_dependency.
              APPEND ls_dependency TO dependencies.
            CATCH cx_root.
              " optional dependency may not exist in the target system
          ENDTRY.
        ENDIF.

        ASSIGN COMPONENT 'TIME_REFERENCES' OF STRUCTURE <ls_content>
          TO <lt_items>.
        IF sy-subrc = 0.
          LOOP AT <lt_items> ASSIGNING <ls_item>.
            UNASSIGN <lv_obj_name>.
            ASSIGN COMPONENT 'SOURCE_TABLE' OF STRUCTURE <ls_item>
              TO <lv_obj_name>.
            IF sy-subrc = 0 AND <lv_obj_name> IS NOT INITIAL.
              TRY.
                  CLEAR ls_dependency.
                  CALL METHOD get_tadir_dependency
                    EXPORTING object = 'TABL' obj_name = <lv_obj_name>
                    RECEIVING dependency = ls_dependency.
                  APPEND ls_dependency TO dependencies.
                CATCH cx_root.
                  " optional dependency may not exist in the target system
              ENDTRY.
            ENDIF.
          ENDLOOP.
        ENDIF.

        UNASSIGN <lt_items>.
        ASSIGN COMPONENT 'CONDITION_FIELDS' OF STRUCTURE <ls_content>
          TO <lt_items>.
        IF sy-subrc = 0.
          LOOP AT <lt_items> ASSIGNING <ls_item>.
            UNASSIGN <lv_obj_name>.
            ASSIGN COMPONENT 'DATA_ELEMENT' OF STRUCTURE <ls_item>
              TO <lv_obj_name>.
            IF sy-subrc = 0 AND <lv_obj_name> IS NOT INITIAL.
              TRY.
                  CLEAR ls_dependency.
                  CALL METHOD get_tadir_dependency
                    EXPORTING object = 'DTEL' obj_name = <lv_obj_name>
                    RECEIVING dependency = ls_dependency.
                  APPEND ls_dependency TO dependencies.
                CATCH cx_root.
                  " optional dependency may not exist in the target system
              ENDTRY.
            ENDIF.

            UNASSIGN <lv_obj_name>.
            ASSIGN COMPONENT 'SOURCE_TABLE' OF STRUCTURE <ls_item>
              TO <lv_obj_name>.
            IF sy-subrc = 0 AND <lv_obj_name> IS NOT INITIAL.
              TRY.
                  CLEAR ls_dependency.
                  CALL METHOD get_tadir_dependency
                    EXPORTING object = 'TABL' obj_name = <lv_obj_name>
                    RECEIVING dependency = ls_dependency.
                  APPEND ls_dependency TO dependencies.
                CATCH cx_root.
                  " optional dependency may not exist in the target system
              ENDTRY.
            ENDIF.
          ENDLOOP.
        ENDIF.
      CATCH cx_root.
        " optional information lifecycle management API may not exist in the target system
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
