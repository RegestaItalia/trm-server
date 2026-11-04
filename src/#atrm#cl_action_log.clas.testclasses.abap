CLASS ltc_action_log DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL DANGEROUS.
  PRIVATE SECTION.
    DATA mv_action_id TYPE /atrm/log_action_id.
    METHODS setup.
    METHODS teardown.
    METHODS lifecycle FOR TESTING.
    METHODS duplicate_start_fails FOR TESTING.
    METHODS invalid_status_fails FOR TESTING.
    METHODS events_need_action FOR TESTING.
ENDCLASS.

CLASS ltc_action_log IMPLEMENTATION.
  METHOD setup.
    TRY.
        mv_action_id = cl_system_uuid=>create_uuid_c32_static( ).
      CATCH cx_uuid_error.
        cl_abap_unit_assert=>fail( msg = 'Could not create test action ID' ).
    ENDTRY.
  ENDMETHOD.

  METHOD teardown.
    DELETE FROM /atrm/log_evt WHERE action_id = mv_action_id.
    DELETE FROM /atrm/log_tr WHERE action_id = mv_action_id.
    DELETE FROM /atrm/log_act WHERE action_id = mv_action_id.
    COMMIT WORK.
  ENDMETHOD.

  METHOD lifecycle.
    DATA: lt_events TYPE /atrm/cl_action_log=>tyt_events,
          ls_event  TYPE /atrm/log_evt_in,
          ls_action TYPE /atrm/log_act,
          ls_link   TYPE /atrm/log_tr,
          lt_rows   TYPE STANDARD TABLE OF /atrm/log_evt,
          ls_row    TYPE /atrm/log_evt.

    TRY.
        /atrm/cl_action_log=>start_action(
          iv_action_id = mv_action_id
          iv_session_id = 'abap-unit-session'
          iv_action_name = 'install'
          iv_package_name = 'abap-unit'
          iv_version = '1.0.0' ).

        ls_event-trkorr = 'TRMK999999'.
        ls_event-event = 'CREATE'.
        APPEND ls_event TO lt_events.
        ls_event-role = 'TADIR'.
        ls_event-event = 'IMPORT'.
        ls_event-target = sy-sysid.
        ls_event-retcode = '0004'.
        APPEND ls_event TO lt_events.
        /atrm/cl_action_log=>add_events( iv_action_id = mv_action_id it_events = lt_events ).

        CLEAR lt_events.
        ls_event-role = 'BKP'.
        ls_event-event = 'RESTORE'.
        APPEND ls_event TO lt_events.
        /atrm/cl_action_log=>add_events( iv_action_id = mv_action_id it_events = lt_events ).

        /atrm/cl_action_log=>end_action(
          iv_action_id = mv_action_id
          iv_status = /atrm/cl_action_log=>c_status-rolled_back ).
      CATCH /atrm/cx_exception.
        cl_abap_unit_assert=>fail( msg = 'Action log operation failed' ).
    ENDTRY.

    SELECT SINGLE * FROM /atrm/log_act INTO ls_action WHERE action_id = mv_action_id.
    cl_abap_unit_assert=>assert_equals( act = ls_action-status exp = 'B' ).
    cl_abap_unit_assert=>assert_equals( act = ls_action-created_by exp = sy-uname ).
    cl_abap_unit_assert=>assert_not_initial( ls_action-started_at ).
    cl_abap_unit_assert=>assert_not_initial( ls_action-ended_at ).

    " The first non-empty role wins.
    SELECT SINGLE * FROM /atrm/log_tr INTO ls_link
      WHERE action_id = mv_action_id AND trkorr = 'TRMK999999'.
    cl_abap_unit_assert=>assert_equals( act = ls_link-role exp = 'TADIR' ).

    SELECT * FROM /atrm/log_evt INTO TABLE lt_rows
      WHERE action_id = mv_action_id ORDER BY seq.
    cl_abap_unit_assert=>assert_equals( act = lines( lt_rows ) exp = 3 ).
    READ TABLE lt_rows INTO ls_row INDEX 3.
    cl_abap_unit_assert=>assert_equals( act = ls_row-seq exp = '000003' ).
    cl_abap_unit_assert=>assert_equals( act = ls_row-event exp = 'RESTORE' ).
  ENDMETHOD.

  METHOD duplicate_start_fails.
    DATA lv_failed TYPE abap_bool.
    TRY.
        /atrm/cl_action_log=>start_action(
          iv_action_id = mv_action_id
          iv_session_id = 'abap-unit-session'
          iv_action_name = 'publish' ).
      CATCH /atrm/cx_exception.
        cl_abap_unit_assert=>fail( msg = 'First start failed' ).
    ENDTRY.
    TRY.
        /atrm/cl_action_log=>start_action(
          iv_action_id = mv_action_id
          iv_session_id = 'abap-unit-session'
          iv_action_name = 'publish' ).
      CATCH /atrm/cx_exception.
        lv_failed = abap_true.
    ENDTRY.
    cl_abap_unit_assert=>assert_equals( act = lv_failed exp = abap_true ).
  ENDMETHOD.

  METHOD invalid_status_fails.
    DATA lv_failed TYPE abap_bool.
    TRY.
        /atrm/cl_action_log=>end_action(
          iv_action_id = mv_action_id
          iv_status = /atrm/cl_action_log=>c_status-running ).
      CATCH /atrm/cx_exception.
        lv_failed = abap_true.
    ENDTRY.
    cl_abap_unit_assert=>assert_equals( act = lv_failed exp = abap_true ).
  ENDMETHOD.

  METHOD events_need_action.
    DATA: lt_events TYPE /atrm/cl_action_log=>tyt_events,
          ls_event  TYPE /atrm/log_evt_in,
          lv_failed TYPE abap_bool.
    ls_event-trkorr = 'TRMK999999'.
    ls_event-event = 'CREATE'.
    APPEND ls_event TO lt_events.
    TRY.
        /atrm/cl_action_log=>add_events( iv_action_id = mv_action_id it_events = lt_events ).
      CATCH /atrm/cx_exception.
        lv_failed = abap_true.
    ENDTRY.
    cl_abap_unit_assert=>assert_equals( act = lv_failed exp = abap_true ).
  ENDMETHOD.
ENDCLASS.

CLASS ltc_log_tables DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS no_client_field FOR TESTING.
ENDCLASS.

CLASS ltc_log_tables IMPLEMENTATION.
  METHOD no_client_field.
    DATA lv_fieldname TYPE dd03l-fieldname.
    SELECT SINGLE fieldname FROM dd03l INTO lv_fieldname
      WHERE tabname IN ('/ATRM/LOG_ACT', '/ATRM/LOG_TR', '/ATRM/LOG_EVT')
        AND fieldname = 'MANDT'
        AND as4local = 'A'.
    cl_abap_unit_assert=>assert_subrc( exp = 4 ).
  ENDMETHOD.
ENDCLASS.
