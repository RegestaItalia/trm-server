CLASS /atrm/cl_object_ssfo DEFINITION
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
    METHODS append_object
      IMPORTING object_type  TYPE trobjtype
                object_name  TYPE string
      CHANGING  dependencies TYPE /atrm/object_dependency_t.
ENDCLASS.



CLASS /atrm/cl_object_ssfo IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA:
      lo_form    TYPE REF TO cl_ssf_fb_smart_form,
      lv_form    TYPE tdsfname,
      lo_ixml    TYPE REF TO if_ixml,
      lo_doc     TYPE REF TO if_ixml_document,
      lo_stream  TYPE REF TO if_ixml_ostream,
      lv_xml     TYPE string,
      lt_results TYPE match_result_tab,
      lv_value   TYPE string.

    FIELD-SYMBOLS:
      <ls_result>   TYPE match_result,
      <ls_submatch> TYPE submatch_result.

    " Serialize the complete form (header, interface, global data, node tree)
    TRY.
        lv_form = me->key-obj_name.
        CREATE OBJECT lo_form.
        lo_form->load( im_formname = lv_form im_active = 'X' ).
        lo_ixml = cl_ixml=>create( ).
        lo_doc = lo_ixml->create_document( ).
        lo_form->xml_download( EXPORTING parent   = lo_doc
                               CHANGING  document = lo_doc ).
        lo_stream = lo_ixml->create_stream_factory( )->create_ostream_cstring( string = lv_xml ).
        lo_doc->render( ostream = lo_stream ).
      CATCH cx_root.
        RETURN.
    ENDTRY.
    CHECK lv_xml IS NOT INITIAL.

    " Form style and node styles
    FIND ALL OCCURRENCES OF REGEX '<(STDSTYLE|STYLE_NAME)>([^<]+)</(STDSTYLE|STYLE_NAME)>' IN lv_xml RESULTS lt_results.
    LOOP AT lt_results ASSIGNING <ls_result>.
      READ TABLE <ls_result>-submatches ASSIGNING <ls_submatch> INDEX 2.
      CHECK sy-subrc = 0.
      lv_value = lv_xml+<ls_submatch>-offset(<ls_submatch>-length).
      append_object( EXPORTING object_type = 'SSST' object_name = lv_value
                     CHANGING  dependencies = dependencies ).
    ENDLOOP.

    " Interface parameter, global data and field-symbol typing
    FIND ALL OCCURRENCES OF REGEX '<TYPENAME>([^<]+)</TYPENAME>' IN lv_xml RESULTS lt_results.
    LOOP AT lt_results ASSIGNING <ls_result>.
      READ TABLE <ls_result>-submatches ASSIGNING <ls_submatch> INDEX 1.
      CHECK sy-subrc = 0.
      lv_value = lv_xml+<ls_submatch>-offset(<ls_submatch>-length).
      append_type( EXPORTING type_name = lv_value
                   CHANGING  dependencies = dependencies ).
    ENDLOOP.

    " Function modules called from program lines and initialization code
    FIND ALL OCCURRENCES OF REGEX `CALL\s+FUNCTION\s+(&apos;|')([^'&]+)` IN lv_xml
      IGNORING CASE RESULTS lt_results.
    LOOP AT lt_results ASSIGNING <ls_result>.
      READ TABLE <ls_result>-submatches ASSIGNING <ls_submatch> INDEX 2.
      CHECK sy-subrc = 0.
      lv_value = lv_xml+<ls_submatch>-offset(<ls_submatch>-length).
      append_function( EXPORTING function_name = lv_value
                       CHANGING  dependencies  = dependencies ).
    ENDLOOP.

    " Text nodes referencing a text module: module name in REF_NAME
    FIND ALL OCCURRENCES OF REGEX '<REF_NAME>([^<]+)</REF_NAME>' IN lv_xml RESULTS lt_results.
    LOOP AT lt_results ASSIGNING <ls_result>.
      READ TABLE <ls_result>-submatches ASSIGNING <ls_submatch> INDEX 1.
      CHECK sy-subrc = 0.
      lv_value = lv_xml+<ls_submatch>-offset(<ls_submatch>-length).
      REPLACE ALL OCCURRENCES OF `&apos;` IN lv_value WITH ``.
      REPLACE ALL OCCURRENCES OF `'` IN lv_value WITH ``.
      append_object( EXPORTING object_type = 'SSFO' object_name = lv_value
                     CHANGING  dependencies = dependencies ).
    ENDLOOP.

    DELETE dependencies WHERE tabname = 'TADIR' AND tabkey = 'R3TRSSFO' && me->key-obj_name.
    SORT dependencies BY tabname tabkey.
    DELETE ADJACENT DUPLICATES FROM dependencies COMPARING tabname tabkey.
  ENDMETHOD.

  METHOD append_object.
    DATA:
      lv_object     TYPE sobj_name,
      ls_dependency TYPE /atrm/object_dependency.

    lv_object = object_name.
    CONDENSE lv_object.
    TRANSLATE lv_object TO UPPER CASE.
    CHECK lv_object IS NOT INITIAL.
    TRY.
        CALL METHOD get_tadir_dependency
          EXPORTING object = object_type obj_name = lv_object
          RECEIVING dependency = ls_dependency.
        APPEND ls_dependency TO dependencies.
      CATCH cx_root.
        " optional dependency may not exist in the target system
    ENDTRY.
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
