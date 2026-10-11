CLASS /atrm/cl_object_edcc DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_edcc IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      ls_object_type TYPE wbobjtype,
      lo_operator    TYPE REF TO object,
      lo_data_model  TYPE REF TO object,
      lr_data        TYPE REF TO data,
      lv_data_type   TYPE string,
      lt_entities    TYPE STANDARD TABLE OF string,
      lt_tables      TYPE STANDARD TABLE OF string,
      lt_classes     TYPE STANDARD TABLE OF string,
      lv_obj_name    TYPE string,
      lv_ddls_name   TYPE sobj_name,
      ls_dependency  TYPE /atrm/object_dependency.

    FIELD-SYMBOLS:
      <ls_data>      TYPE any,
      <ls_content>   TYPE any,
      <lt_items>     TYPE ANY TABLE,
      <ls_item>      TYPE any,
      <lv_value>     TYPE any.

    TRY.
        ls_object_type-objtype_tr = 'EDCC'.

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

        ASSIGN COMPONENT 'ADDITIONAL_SELECTION_FIELDS'
          OF STRUCTURE <ls_content> TO <lt_items>.
        IF sy-subrc = 0.
          LOOP AT <lt_items> ASSIGNING <ls_item>.
            UNASSIGN <lv_value>.
            ASSIGN COMPONENT 'VIEW_NAME' OF STRUCTURE <ls_item> TO <lv_value>.
            IF sy-subrc = 0 AND <lv_value> IS NOT INITIAL.
              APPEND <lv_value> TO lt_entities.
            ENDIF.
          ENDLOOP.
        ENDIF.

        UNASSIGN <lt_items>.
        ASSIGN COMPONENT 'TAX_AUTHORITY_TABLES'
          OF STRUCTURE <ls_content> TO <lt_items>.
        IF sy-subrc = 0.
          LOOP AT <lt_items> ASSIGNING <ls_item>.
            UNASSIGN <lv_value>.
            ASSIGN COMPONENT 'NAME' OF STRUCTURE <ls_item> TO <lv_value>.
            IF sy-subrc = 0 AND <lv_value> IS NOT INITIAL.
              APPEND <lv_value> TO lt_tables.
            ENDIF.
          ENDLOOP.
        ENDIF.

        UNASSIGN <lt_items>.
        ASSIGN COMPONENT 'COMPARISON_TYPES_AND_EVENTS-COMPARISON_TYPES'
          OF STRUCTURE <ls_content> TO <lt_items>.
        IF sy-subrc = 0.
          LOOP AT <lt_items> ASSIGNING <ls_item>.
            UNASSIGN <lv_value>.
            ASSIGN COMPONENT 'IMPLEMENTING_CLASS'
              OF STRUCTURE <ls_item> TO <lv_value>.
            IF sy-subrc = 0 AND <lv_value> IS NOT INITIAL.
              APPEND <lv_value> TO lt_classes.
            ENDIF.
            UNASSIGN <lv_value>.
            ASSIGN COMPONENT 'DATA_SOURCE'
              OF STRUCTURE <ls_item> TO <lv_value>.
            IF sy-subrc = 0 AND <lv_value> IS NOT INITIAL.
              APPEND <lv_value> TO lt_entities.
            ENDIF.
          ENDLOOP.
        ENDIF.

        UNASSIGN <lt_items>.
        ASSIGN COMPONENT 'COMPARISON_TYPES_AND_EVENTS-INCONSISTENCY_CATEGORIES'
          OF STRUCTURE <ls_content> TO <lt_items>.
        IF sy-subrc = 0.
          LOOP AT <lt_items> ASSIGNING <ls_item>.
            UNASSIGN <lv_value>.
            ASSIGN COMPONENT 'COUNTRY_VIEW_EXTENSION'
              OF STRUCTURE <ls_item> TO <lv_value>.
            IF sy-subrc = 0 AND <lv_value> IS NOT INITIAL.
              APPEND <lv_value> TO lt_entities.
            ENDIF.
          ENDLOOP.
        ENDIF.

        LOOP AT lt_classes INTO lv_obj_name.
          TRY.
              CLEAR ls_dependency.
              CALL METHOD get_tadir_dependency
                EXPORTING object = 'CLAS' obj_name = lv_obj_name
                RECEIVING dependency = ls_dependency.
              APPEND ls_dependency TO dependencies.
            CATCH cx_root.
              " optional dependency may not exist in the target system
          ENDTRY.
        ENDLOOP.

        LOOP AT lt_tables INTO lv_obj_name.
          TRY.
              CLEAR ls_dependency.
              CALL METHOD get_tadir_dependency
                EXPORTING object = 'TABL' obj_name = lv_obj_name
                RECEIVING dependency = ls_dependency.
              APPEND ls_dependency TO dependencies.
            CATCH cx_root.
              " optional dependency may not exist in the target system
          ENDTRY.
        ENDLOOP.

        LOOP AT lt_entities INTO lv_obj_name.
          CLEAR lv_ddls_name.
          TRY.
              SELECT SINGLE ddlname
                FROM ddldependency
                INTO lv_ddls_name
                WHERE objectname = lv_obj_name
                  AND objecttype = 'STOB'
                  AND state = 'A'.
              IF lv_ddls_name IS NOT INITIAL.
                CLEAR ls_dependency.
                CALL METHOD get_tadir_dependency
                  EXPORTING object = 'DDLS' obj_name = lv_ddls_name
                  RECEIVING dependency = ls_dependency.
                APPEND ls_dependency TO dependencies.
              ELSE.
                CLEAR ls_dependency.
                CALL METHOD get_tadir_dependency
                  EXPORTING object = 'TABL' obj_name = lv_obj_name
                  RECEIVING dependency = ls_dependency.
                APPEND ls_dependency TO dependencies.
              ENDIF.
            CATCH cx_root.
              " optional dependency may not exist in the target system
          ENDTRY.
        ENDLOOP.
      CATCH cx_root.
        " optional electronic document consistency API may not exist in the target system
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
