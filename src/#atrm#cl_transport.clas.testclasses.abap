CLASS ltc_transport DEFINITION FOR TESTING
  "#AU Risk_Level Harmless
  "#AU Duration Short
.
  PRIVATE SECTION.
    METHODS constructor_keeps_request FOR TESTING.
    METHODS shi3_untouched_without_shi3 FOR TESTING RAISING /atrm/cx_exception.
    METHODS shi3_unknown_structure_raises FOR TESTING.
    METHODS fdt0_ignores_other_objects FOR TESTING RAISING /atrm/cx_exception.
    METHODS tp_result_xml_shape FOR TESTING.
    METHODS unknown_fm_param_not_bound FOR TESTING.
    METHODS stms_result_copied_by_name FOR TESTING.
ENDCLASS.

CLASS /atrm/cl_transport DEFINITION LOCAL FRIENDS ltc_transport.

CLASS ltc_transport IMPLEMENTATION.
  METHOD constructor_keeps_request.
    DATA: lo_transport TYPE REF TO /atrm/cl_transport,
          lv_actual    TYPE trkorr,
          lv_expected  TYPE trkorr.
    lv_expected = 'DEVK900001'.
    CREATE OBJECT lo_transport
      EXPORTING
        trkorr = lv_expected.
    lv_actual = lo_transport->get_trkorr( ).
    cl_aunit_assert=>assert_equals(
      act = lv_actual
      exp = lv_expected
    ).
  ENDMETHOD.

  METHOD shi3_untouched_without_shi3.
    DATA: lo_transport TYPE REF TO /atrm/cl_transport,
          lt_e071      TYPE /atrm/cl_transport=>tyt_e071,
          lt_expected  TYPE /atrm/cl_transport=>tyt_e071,
          lt_e071k     TYPE /atrm/cl_transport=>tyt_e071k,
          ls_e071      TYPE e071.
    CREATE OBJECT lo_transport
      EXPORTING
        trkorr = 'DEVK900001'.
    ls_e071-pgmid = 'R3TR'.
    ls_e071-object = 'TRAN'.
    ls_e071-obj_name = 'ZTEST'.
    APPEND ls_e071 TO lt_e071.
    ls_e071-object = 'DEVC'.
    ls_e071-obj_name = 'ZPACKAGE'.
    APPEND ls_e071 TO lt_e071.
    lt_expected = lt_e071.
    CALL METHOD lo_transport->complete_shi3_entries
      CHANGING
        e071  = lt_e071
        e071k = lt_e071k.
    cl_aunit_assert=>assert_equals( act = lt_e071 exp = lt_expected ).
    cl_aunit_assert=>assert_initial( act = lt_e071k ).
  ENDMETHOD.

  METHOD shi3_unknown_structure_raises.
    DATA: lo_transport TYPE REF TO /atrm/cl_transport,
          lt_e071      TYPE /atrm/cl_transport=>tyt_e071,
          lt_e071k     TYPE /atrm/cl_transport=>tyt_e071k,
          ls_e071      TYPE e071.
    CREATE OBJECT lo_transport
      EXPORTING
        trkorr = 'DEVK900001'.
    ls_e071-pgmid = 'R3TR'.
    ls_e071-object = 'SHI3'.
    ls_e071-obj_name = 'ZATRM_UNIT_NOT_EXISTING_SHI3'.
    APPEND ls_e071 TO lt_e071.
    TRY.
        CALL METHOD lo_transport->complete_shi3_entries
          CHANGING
            e071  = lt_e071
            e071k = lt_e071k.
        cl_aunit_assert=>fail( msg = 'Exception expected for unknown structure' ).
      CATCH /atrm/cx_exception.
    ENDTRY.
  ENDMETHOD.

  METHOD fdt0_ignores_other_objects.
    DATA: lo_transport TYPE REF TO /atrm/cl_transport,
          lt_e071      TYPE /atrm/cl_transport=>tyt_e071,
          ls_e071      TYPE e071.
    CREATE OBJECT lo_transport
      EXPORTING
        trkorr = 'DEVK900001'.
    ls_e071-pgmid = 'R3TR'.
    ls_e071-object = 'TRAN'.
    ls_e071-obj_name = 'ZTEST'.
    APPEND ls_e071 TO lt_e071.
    ls_e071-object = 'FDT0'.
    ls_e071-obj_name = 'ZATRM_UNIT_NOT_EXISTING_FDT0'.
    APPEND ls_e071 TO lt_e071.
    " no BRF+ application with that name: nothing to record, no exception
    lo_transport->record_fdt0_entries( lt_e071 ).
  ENDMETHOD.

  METHOD tp_result_xml_shape.
    " trm-core parses /ATRM/IMPORT_TR test_result: element names must stay those
    " of STMS_TP_IMPORT (rows of TP_STDOUT/TP_LOGPTR named after the row type)
    DATA: ls_import TYPE /atrm/cl_transport=>ty_tp_result,
          ls_stdout TYPE tpstdout,
          lv_xml    TYPE string.
    ls_import-request-trkorr = 'DEVK900001'.
    ls_import-tp_ret_code = '0004'.
    ls_stdout-line = 'tp finished'.
    APPEND ls_stdout TO ls_import-tp_stdout.
    ls_import-alert-msgid = 'XT'.
    CALL TRANSFORMATION id
      SOURCE import = ls_import
      RESULT XML lv_xml.
    FIND '<IMPORT><REQUEST><TRKORR>DEVK900001</TRKORR>' IN lv_xml.
    cl_aunit_assert=>assert_subrc( act = sy-subrc exp = 0 msg = 'REQUEST-TRKORR' ).
    FIND '<TP_RET_CODE>0004</TP_RET_CODE>' IN lv_xml.
    cl_aunit_assert=>assert_subrc( act = sy-subrc exp = 0 msg = 'TP_RET_CODE' ).
    FIND '<TP_STDOUT><TPSTDOUT><LINE>tp finished</LINE></TPSTDOUT></TP_STDOUT>' IN lv_xml.
    cl_aunit_assert=>assert_subrc( act = sy-subrc exp = 0 msg = 'TP_STDOUT rows' ).
    FIND '<TP_LOGPTR/>' IN lv_xml.
    cl_aunit_assert=>assert_subrc( act = sy-subrc exp = 0 msg = 'TP_LOGPTR' ).
    FIND '<ALERT><ID' IN lv_xml.
    cl_aunit_assert=>assert_subrc( act = sy-subrc exp = 0 msg = 'ALERT' ).
  ENDMETHOD.

  METHOD unknown_fm_param_not_bound.
    DATA lr_data TYPE REF TO data.
    lr_data = /atrm/cl_transport=>create_fm_param_data(
      funcname  = 'TMS_MGR_IMPORT_TR_REQUEST'
      parameter = 'ZZ_NOT_EXISTING' ).
    cl_aunit_assert=>assert_not_bound( act = lr_data ).
  ENDMETHOD.

  METHOD stms_result_copied_by_name.
    " Where the function has ET_TP_IMPORTS (type group STMS) it must resolve,
    " and a row is copied into ty_tp_result by component name
    DATA: lr_data   TYPE REF TO data,
          lr_row    TYPE REF TO data,
          ls_result TYPE /atrm/cl_transport=>ty_tp_result,
          ls_stdout TYPE tpstdout.
    FIELD-SYMBOLS: <lt_rows>  TYPE STANDARD TABLE,
                   <ls_row>   TYPE any,
                   <lv_value> TYPE any,
                   <lt_lines> TYPE STANDARD TABLE.

    IF /atrm/cl_transport=>fm_has_parameter(
         funcname = 'TMS_MGR_IMPORT_TR_REQUEST' parameter = 'ET_TP_IMPORTS' ) IS INITIAL.
      RETURN. " old release: the classic outputs are used instead
    ENDIF.
    lr_data = /atrm/cl_transport=>create_fm_param_data(
      funcname  = 'TMS_MGR_IMPORT_TR_REQUEST'
      parameter = 'ET_TP_IMPORTS' ).
    cl_aunit_assert=>assert_bound( act = lr_data msg = 'STMS_TP_IMPORTS not resolved' ).
    ASSIGN lr_data->* TO <lt_rows>.
    CREATE DATA lr_row LIKE LINE OF <lt_rows>.
    ASSIGN lr_row->* TO <ls_row>.
    ASSIGN COMPONENT 'REQUEST-TRKORR' OF STRUCTURE <ls_row> TO <lv_value>.
    IF sy-subrc <> 0.
      ASSIGN COMPONENT 'REQUEST' OF STRUCTURE <ls_row> TO <lv_value>.
      ASSIGN COMPONENT 'TRKORR' OF STRUCTURE <lv_value> TO <lv_value>.
    ENDIF.
    <lv_value> = 'DEVK900002'.
    ASSIGN COMPONENT 'TP_RET_CODE' OF STRUCTURE <ls_row> TO <lv_value>.
    <lv_value> = '0008'.
    ASSIGN COMPONENT 'TP_STDOUT' OF STRUCTURE <ls_row> TO <lt_lines>.
    ls_stdout-line = 'stdout line'.
    APPEND ls_stdout TO <lt_lines>.

    CALL METHOD /atrm/cl_transport=>copy_tp_result
      EXPORTING source = <ls_row>
      CHANGING  result = ls_result.

    cl_aunit_assert=>assert_equals( act = ls_result-request-trkorr exp = 'DEVK900002' ).
    cl_aunit_assert=>assert_equals( act = ls_result-tp_ret_code exp = '0008' ).
    READ TABLE ls_result-tp_stdout INTO ls_stdout INDEX 1.
    cl_aunit_assert=>assert_equals( act = ls_stdout-line exp = 'stdout line' ).
  ENDMETHOD.
ENDCLASS.
