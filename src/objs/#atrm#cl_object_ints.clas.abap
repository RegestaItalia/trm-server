CLASS /atrm/cl_object_ints DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_ints IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    TYPES:
      BEGIN OF ty_reference,
        path   TYPE string,
        object TYPE trobjtype,
      END OF ty_reference.

    DATA:
      ls_object_type TYPE wbobjtype,
      lo_operator    TYPE REF TO object,
      lo_data_model  TYPE REF TO object,
      lr_data        TYPE REF TO data,
      lv_data_type   TYPE string,
      lt_references  TYPE STANDARD TABLE OF ty_reference,
      ls_reference   TYPE ty_reference,
      lv_ddls_name   TYPE sobj_name,
      lv_table_name  TYPE tabname,
      ls_dependency  TYPE /atrm/object_dependency.

    FIELD-SYMBOLS:
      <ls_data>      TYPE any,
      <ls_content>   TYPE any,
      <lt_items>     TYPE ANY TABLE,
      <ls_item>      TYPE any,
      <lv_object>    TYPE any,
      <lv_obj_name>  TYPE any.

    TRY.
        ls_object_type-objtype_tr = 'INTS'.

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

        ls_reference-path = 'CLASS_INFORMATION-PREDICTION_CLASS'.
        ls_reference-object = 'CLAS'.
        APPEND ls_reference TO lt_references.
        ls_reference-path = 'CLASS_INFORMATION-PREREQUISITE_CHECK_CLASS'.
        ls_reference-object = 'CLAS'.
        APPEND ls_reference TO lt_references.
        ls_reference-path = 'TURNKEY_INFORMATION-TURNKEY_CLASS'.
        ls_reference-object = 'CLAS'.
        APPEND ls_reference TO lt_references.
        ls_reference-path = 'CONNECTION_INFORMATION-OAUTH_PROFILE'.
        ls_reference-object = 'OA2P'.
        APPEND ls_reference TO lt_references.

        LOOP AT lt_references INTO ls_reference.
          UNASSIGN <lv_obj_name>.
          ASSIGN COMPONENT ls_reference-path OF STRUCTURE <ls_content>
            TO <lv_obj_name>.
          CHECK sy-subrc = 0.
          CHECK <lv_obj_name> IS NOT INITIAL.

          TRY.
              CLEAR ls_dependency.
              CALL METHOD get_tadir_dependency
                EXPORTING object = ls_reference-object obj_name = <lv_obj_name>
                RECEIVING dependency = ls_dependency.
              APPEND ls_dependency TO dependencies.
            CATCH cx_root.
              " optional dependency may not exist in the target system
          ENDTRY.
        ENDLOOP.

        ASSIGN COMPONENT 'SCENARIO_DDL_OBJECTS' OF STRUCTURE <ls_content>
          TO <lt_items>.
        IF sy-subrc = 0.
          LOOP AT <lt_items> ASSIGNING <ls_item>.
            UNASSIGN: <lv_object>, <lv_obj_name>.
            ASSIGN COMPONENT 'OBJECT_TYPE' OF STRUCTURE <ls_item>
              TO <lv_object>.
            ASSIGN COMPONENT 'OBJECT_NAME' OF STRUCTURE <ls_item>
              TO <lv_obj_name>.
            IF <lv_object> IS ASSIGNED
              AND <lv_obj_name> IS ASSIGNED
              AND <lv_object> IS NOT INITIAL
              AND <lv_obj_name> IS NOT INITIAL.
              TRY.
                  CLEAR ls_dependency.
                  CALL METHOD get_tadir_dependency
                    EXPORTING object = <lv_object> obj_name = <lv_obj_name>
                    RECEIVING dependency = ls_dependency.
                  APPEND ls_dependency TO dependencies.
                CATCH cx_root.
                  " optional dependency may not exist in the target system
              ENDTRY.
            ENDIF.
          ENDLOOP.
        ENDIF.

        UNASSIGN <lt_items>.
        ASSIGN COMPONENT 'BINDINGS' OF STRUCTURE <ls_content> TO <lt_items>.
        IF sy-subrc = 0.
          LOOP AT <lt_items> ASSIGNING <ls_item>.
            UNASSIGN <lv_obj_name>.
            ASSIGN COMPONENT 'REFERENCE_OBJECT' OF STRUCTURE <ls_item>
              TO <lv_obj_name>.
            CHECK sy-subrc = 0.
            CHECK <lv_obj_name> IS NOT INITIAL.

            CLEAR lv_ddls_name.
            lv_table_name = 'DDLDEPENDENCY'.
            TRY.
                SELECT SINGLE ddlname
                  FROM (lv_table_name)
                  INTO lv_ddls_name
                  WHERE objectname = <lv_obj_name>
                    AND objecttype = 'STOB'
                    AND state = 'A'.
                CHECK lv_ddls_name IS NOT INITIAL.

                CLEAR ls_dependency.
                CALL METHOD get_tadir_dependency
                  EXPORTING object = 'DDLS' obj_name = lv_ddls_name
                  RECEIVING dependency = ls_dependency.
                APPEND ls_dependency TO dependencies.
              CATCH cx_root.
                " optional dependency may not exist in the target system
            ENDTRY.
          ENDLOOP.
        ENDIF.
      CATCH cx_root.
        " optional intelligent scenario API may not exist in the target system
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
