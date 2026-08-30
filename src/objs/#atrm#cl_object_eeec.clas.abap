CLASS /atrm/cl_object_eeec DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_eeec IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      ls_object_type TYPE wbobjtype,
      lo_operator    TYPE REF TO object,
      lo_data_model  TYPE REF TO object,
      lr_data        TYPE REF TO data,
      lv_data_type   TYPE string,
      lv_ddls_name   TYPE sobj_name,
      lv_table_name  TYPE tabname,
      ls_dependency  TYPE /atrm/object_dependency.

    FIELD-SYMBOLS:
      <ls_data>         TYPE any,
      <ls_content>      TYPE any,
      <lv_class_name>   TYPE any,
      <lt_event_types>  TYPE ANY TABLE,
      <ls_event_type>   TYPE any,
      <lv_entity_name>  TYPE any.

    TRY.
        ls_object_type-objtype_tr = 'EEEC'.
        ls_object_type-subtype_wb = 'EVC'.

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

        ASSIGN COMPONENT 'CONSUMER_CLASS' OF STRUCTURE <ls_content>
          TO <lv_class_name>.
        IF sy-subrc = 0 AND <lv_class_name> IS NOT INITIAL.
          TRY.
              CALL METHOD get_tadir_dependency
                EXPORTING object = 'CLAS' obj_name = <lv_class_name>
                RECEIVING dependency = ls_dependency.
              APPEND ls_dependency TO dependencies.
            CATCH cx_root.
              " optional dependency may not exist in the target system
          ENDTRY.
        ENDIF.

        ASSIGN COMPONENT 'DESCRIPTOR_CLASS' OF STRUCTURE <ls_content>
          TO <lv_class_name>.
        IF sy-subrc = 0 AND <lv_class_name> IS NOT INITIAL.
          TRY.
              CLEAR ls_dependency.
              CALL METHOD get_tadir_dependency
                EXPORTING object = 'CLAS' obj_name = <lv_class_name>
                RECEIVING dependency = ls_dependency.
              APPEND ls_dependency TO dependencies.
            CATCH cx_root.
              " optional dependency may not exist in the target system
          ENDTRY.
        ENDIF.

        ASSIGN COMPONENT 'EVENT_TYPES' OF STRUCTURE <ls_content>
          TO <lt_event_types>.
        IF sy-subrc = 0.
          LOOP AT <lt_event_types> ASSIGNING <ls_event_type>.
            ASSIGN COMPONENT 'ENTITY_NAME' OF STRUCTURE <ls_event_type>
              TO <lv_entity_name>.
            CHECK sy-subrc = 0.
            CHECK <lv_entity_name> IS NOT INITIAL.

            CLEAR lv_ddls_name.
            lv_table_name = 'DDLDEPENDENCY'.
            TRY.
                SELECT SINGLE ddlname
                  FROM (lv_table_name)
                  INTO lv_ddls_name
                  WHERE objectname = <lv_entity_name>
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
        " optional event consumption API may not exist in the target system
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
