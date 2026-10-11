CLASS /atrm/cl_object_sfpi DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS append_type
      IMPORTING type_name    TYPE string
      CHANGING  dependencies TYPE /atrm/object_dependency_t.
    METHODS append_function
      IMPORTING function_name TYPE string
      CHANGING  dependencies  TYPE /atrm/object_dependency_t.
ENDCLASS.



CLASS /atrm/cl_object_sfpi IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      lv_name    TYPE string,
      lv_where   TYPE string,
      lv_table   TYPE tabname,
      lv_raw     TYPE xstring,
      lv_xml     TYPE string,
      lt_results TYPE match_result_tab,
      lv_value   TYPE string.

    FIELD-SYMBOLS:
      <ls_result>   TYPE match_result,
      <ls_submatch> TYPE submatch_result.

    lv_name = me->key-obj_name.
    REPLACE ALL OCCURRENCES OF `'` IN lv_name WITH `''`.

    " Serialized interface (asXML): use the active version, else the saved one
    TRY.
        lv_table = 'FPINTERFACE'.
        CONCATENATE `NAME = '` lv_name `' AND STATE = 'A'` INTO lv_where.
        SELECT SINGLE ('INTERFACE') FROM (lv_table) INTO lv_raw WHERE (lv_where).
        IF sy-subrc <> 0.
          CONCATENATE `NAME = '` lv_name `' AND STATE = 'I'` INTO lv_where.
          SELECT SINGLE ('INTERFACE') FROM (lv_table) INTO lv_raw WHERE (lv_where).
        ENDIF.
        CHECK lv_raw IS NOT INITIAL.
        lv_xml = cl_abap_codepage=>convert_from( lv_raw ).
      CATCH cx_root.
        RETURN.
    ENDTRY.

    " Parameter, global data and field-symbol typing
    FIND ALL OCCURRENCES OF REGEX '<TYPENAME>([^<]+)</TYPENAME>' IN lv_xml RESULTS lt_results.
    LOOP AT lt_results ASSIGNING <ls_result>.
      READ TABLE <ls_result>-submatches ASSIGNING <ls_submatch> INDEX 1.
      CHECK sy-subrc = 0.
      lv_value = lv_xml+<ls_submatch>-offset(<ls_submatch>-length).
      append_type(
        EXPORTING type_name    = lv_value
        CHANGING  dependencies = dependencies ).
    ENDLOOP.

    " Function modules called from initialization code and form routines
    FIND ALL OCCURRENCES OF REGEX `CALL\s+FUNCTION\s+(&apos;|')([^'&]+)` IN lv_xml
      IGNORING CASE RESULTS lt_results.
    LOOP AT lt_results ASSIGNING <ls_result>.
      READ TABLE <ls_result>-submatches ASSIGNING <ls_submatch> INDEX 2.
      CHECK sy-subrc = 0.
      lv_value = lv_xml+<ls_submatch>-offset(<ls_submatch>-length).
      append_function(
        EXPORTING function_name = lv_value
        CHANGING  dependencies  = dependencies ).
    ENDLOOP.

    SORT dependencies BY tabname tabkey.
    DELETE ADJACENT DUPLICATES FROM dependencies COMPARING tabname tabkey.
  ENDMETHOD.

  METHOD append_type.
    DATA:
      lv_name       TYPE string,
      lv_object     TYPE sobj_name,
      lt_types      TYPE STANDARD TABLE OF trobjtype WITH DEFAULT KEY,
      lv_type       TYPE trobjtype,
      ls_dependency TYPE /atrm/object_dependency.

    lv_name = type_name.
    CONDENSE lv_name.
    TRANSLATE lv_name TO UPPER CASE.
    " A component reference (TYPE-FIELD, CLASS=>TYPE) depends on its container
    FIND REGEX '^([A-Z0-9_/]+)' IN lv_name SUBMATCHES lv_object.
    CHECK sy-subrc = 0 AND lv_object IS NOT INITIAL.

    APPEND 'TABL' TO lt_types.
    APPEND 'TTYP' TO lt_types.
    APPEND 'DTEL' TO lt_types.
    APPEND 'CLAS' TO lt_types.
    APPEND 'INTF' TO lt_types.
    APPEND 'VIEW' TO lt_types.
    LOOP AT lt_types INTO lv_type.
      TRY.
          CLEAR ls_dependency.
          CALL METHOD get_tadir_dependency
            EXPORTING object = lv_type obj_name = lv_object
            RECEIVING dependency = ls_dependency.
          APPEND ls_dependency TO dependencies.
          RETURN.
        CATCH cx_root.
          " try the next repository type
      ENDTRY.
    ENDLOOP.

    TRY.
        CLEAR ls_dependency.
        CALL METHOD get_cds_dependency
          EXPORTING entity = lv_object
          IMPORTING dependency = ls_dependency.
        IF ls_dependency IS NOT INITIAL.
          APPEND ls_dependency TO dependencies.
        ENDIF.
      CATCH cx_root.
        " built-in or unknown type
    ENDTRY.
  ENDMETHOD.

  METHOD append_function.
    DATA:
      lv_name  TYPE string,
      lv_where TYPE string.

    lv_name = function_name.
    CONDENSE lv_name.
    TRANSLATE lv_name TO UPPER CASE.
    CHECK lv_name IS NOT INITIAL.
    REPLACE ALL OCCURRENCES OF `'` IN lv_name WITH `''`.
    CONCATENATE `FUNCNAME = '` lv_name `'` INTO lv_where.

    append_table_dependencies(
      EXPORTING table_name   = 'V_FDIR'
                where_clause = lv_where
                object_field = 'AREA'
                object_type  = 'FUGR'
      CHANGING  dependencies = dependencies ).
    append_table_dependencies(
      EXPORTING table_name   = 'V_FDIR'
                where_clause = lv_where
                object_field = 'FUNCNAME'
                object_type  = 'FUNC'
      CHANGING  dependencies = dependencies ).
  ENDMETHOD.

ENDCLASS.
