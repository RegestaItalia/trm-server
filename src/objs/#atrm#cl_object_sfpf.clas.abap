CLASS /atrm/cl_object_sfpf DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /atrm/cl_object_sfpf IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      ls_object_type TYPE wbobjtype,
      lo_operator    TYPE REF TO object,
      lo_data_model  TYPE REF TO object,
      lr_data        TYPE REF TO data,
      lv_data_type   TYPE string,
      lv_name        TYPE string,
      lv_where       TYPE string,
      lv_table       TYPE tabname,
      lr_rows        TYPE REF TO data,
      lv_raw         TYPE xstring,
      lv_xml         TYPE string,
      lt_results     TYPE match_result_tab,
      lv_value       TYPE sobj_name,
      lv_type        TYPE trobjtype,
      ls_dependency  TYPE /atrm/object_dependency.

    FIELD-SYMBOLS:
      <ls_data>      TYPE any,
      <ls_content>   TYPE any,
      <lv_obj_name>  TYPE any,
      <lt_rows>      TYPE STANDARD TABLE,
      <ls_result>    TYPE match_result,
      <ls_submatch>  TYPE submatch_result.

    TRY.
        ls_object_type-objtype_tr = 'SFPF'.
        ls_object_type-subtype_wb = ''.

        CALL METHOD ('CL_WB_OBJECT_OPERATOR')=>('CREATE_INSTANCE')
          EXPORTING object_type = ls_object_type object_key = me->key-obj_name
          RECEIVING result = lo_operator.
        CALL METHOD lo_operator->('IF_WB_OBJECT_OPERATOR~READ')
          EXPORTING version = 'A' data_selection = 'AL'
          IMPORTING eo_object_data = lo_data_model.
        CALL METHOD lo_data_model->('IF_WB_OBJECT_DATA_MODEL~GET_DATATYPE_NAME')
          EXPORTING p_data_selection = 'AL'
          RECEIVING result = lv_data_type.
        IF lv_data_type IS NOT INITIAL.
          CREATE DATA lr_data TYPE (lv_data_type).
          ASSIGN lr_data->* TO <ls_data>.
          CALL METHOD lo_data_model->('IF_WB_OBJECT_DATA_MODEL~GET_SELECTED_DATA')
            EXPORTING p_data_selection = 'AL'
            IMPORTING p_data = <ls_data>.

          ASSIGN COMPONENT 'CONTENT' OF STRUCTURE <ls_data> TO <ls_content>.
          IF sy-subrc <> 0.
            ASSIGN <ls_data> TO <ls_content>.
          ENDIF.

          ASSIGN COMPONENT 'GENERAL_INFORMATION-DATA_PROVIDER' OF STRUCTURE <ls_content>
            TO <lv_obj_name>.
          IF sy-subrc = 0 AND <lv_obj_name> IS NOT INITIAL.
            CALL METHOD get_tadir_dependency
              EXPORTING object = 'SRVD' obj_name = <lv_obj_name>
              RECEIVING dependency = ls_dependency.
            APPEND ls_dependency TO dependencies.
          ENDIF.
        ENDIF.
      CATCH cx_root.
        " optional form template API may not exist in the target system
    ENDTRY.

    " Form header: interface and inbound (offline) handler class.
    " Use the active row; fall back to the saved inactive row.
    lv_name = me->key-obj_name.
    REPLACE ALL OCCURRENCES OF `'` IN lv_name WITH `''`.
    TRY.
        lv_table = 'FPCONTEXT'.
        CREATE DATA lr_rows TYPE STANDARD TABLE OF (lv_table).
        ASSIGN lr_rows->* TO <lt_rows>.
        CONCATENATE `NAME = '` lv_name `' AND STATE = 'A'` INTO lv_where.
        SELECT * FROM (lv_table) INTO TABLE <lt_rows> WHERE (lv_where).
        IF sy-subrc <> 0.
          CONCATENATE `NAME = '` lv_name `' AND STATE = 'I'` INTO lv_where.
        ENDIF.
      CATCH cx_root.
        CONCATENATE `NAME = '` lv_name `' AND STATE = 'A'` INTO lv_where.
    ENDTRY.

    append_table_dependencies(
      EXPORTING table_name   = 'FPCONTEXT'
                where_clause = lv_where
                object_field = 'INTERFACE'
                object_type  = 'SFPI'
      CHANGING  dependencies = dependencies ).
    append_table_dependencies(
      EXPORTING table_name   = 'FPCONTEXT'
                where_clause = lv_where
                object_field = 'OFFLINE_HANDLER'
                object_type  = 'CLAS'
      CHANGING  dependencies = dependencies ).

    " Form context (asXML): text-module nodes (TEXT_NAME -> SSFO) and their styles (STYLE_NAME -> SSST)
    TRY.
        lv_table = 'FPCONTEXT'.
        SELECT SINGLE ('CONTEXT') FROM (lv_table) INTO lv_raw WHERE (lv_where).
        IF sy-subrc = 0 AND lv_raw IS NOT INITIAL.
          lv_xml = cl_abap_codepage=>convert_from( lv_raw ).
        ENDIF.
      CATCH cx_root.
        CLEAR lv_xml.
    ENDTRY.
    IF lv_xml IS NOT INITIAL.
      FIND ALL OCCURRENCES OF REGEX '<(TEXT_NAME|STYLE_NAME)>([^<]+)</(TEXT_NAME|STYLE_NAME)>' IN lv_xml RESULTS lt_results.
      LOOP AT lt_results ASSIGNING <ls_result>.
        READ TABLE <ls_result>-submatches ASSIGNING <ls_submatch> INDEX 1.
        CHECK sy-subrc = 0.
        IF lv_xml+<ls_submatch>-offset(<ls_submatch>-length) = 'TEXT_NAME'.
          lv_type = 'SSFO'.
        ELSE.
          lv_type = 'SSST'.
        ENDIF.
        READ TABLE <ls_result>-submatches ASSIGNING <ls_submatch> INDEX 2.
        CHECK sy-subrc = 0.
        lv_value = lv_xml+<ls_submatch>-offset(<ls_submatch>-length).
        TRANSLATE lv_value TO UPPER CASE.
        TRY.
            CLEAR ls_dependency.
            CALL METHOD get_tadir_dependency
              EXPORTING object = lv_type obj_name = lv_value
              RECEIVING dependency = ls_dependency.
            APPEND ls_dependency TO dependencies.
          CATCH cx_root.
            " optional dependency may not exist in the target system
        ENDTRY.
      ENDLOOP.
    ENDIF.

    SORT dependencies BY tabname tabkey.
    DELETE ADJACENT DUPLICATES FROM dependencies COMPARING tabname tabkey.
  ENDMETHOD.

ENDCLASS.
