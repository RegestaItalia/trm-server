CLASS /atrm/cl_object_chdo DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_chdo IMPLEMENTATION.

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
      <lv_obj_name>   TYPE any,
      <lt_tables>     TYPE ANY TABLE,
      <ls_table>      TYPE any.

    TRY.
        ls_object_type-objtype_tr = 'CHDO'.
        ls_object_type-subtype_wb = 'CHD'.

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

        ASSIGN COMPONENT 'GENERAL_INFORMATION-GENERATED_OBJECT'
          OF STRUCTURE <ls_content> TO <lv_obj_name>.
        IF sy-subrc = 0 AND <lv_obj_name> IS NOT INITIAL.
          TRY.
              CALL METHOD get_tadir_dependency
                EXPORTING object = 'CLAS' obj_name = <lv_obj_name>
                RECEIVING dependency = ls_dependency.
              APPEND ls_dependency TO dependencies.
            CATCH cx_root.
              " optional dependency may not exist in the target system
          ENDTRY.
        ENDIF.

        ASSIGN COMPONENT 'TABLES_AND_STRUCTURES' OF STRUCTURE <ls_content>
          TO <lt_tables>.
        IF sy-subrc = 0.
          LOOP AT <lt_tables> ASSIGNING <ls_table>.
            ASSIGN COMPONENT 'NAME' OF STRUCTURE <ls_table> TO <lv_obj_name>.
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

            ASSIGN COMPONENT 'REFERENCE_TABLE' OF STRUCTURE <ls_table>
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
        " optional change document API may not exist in the target system
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
