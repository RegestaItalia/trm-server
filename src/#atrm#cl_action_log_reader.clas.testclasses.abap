CLASS ltc_attribution DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    DATA mt_links TYPE /atrm/cl_action_log_reader=>tyt_links.
    METHODS setup.
    METHODS add_row
      IMPORTING iv_trkorr  TYPE trkorr
                iv_time    TYPE tstamp
                iv_step    TYPE trtpstep
                iv_retcode TYPE trretcode DEFAULT '0000'
                iv_tpstat  TYPE trtptstamp OPTIONAL
      CHANGING  ct_rows    TYPE /atrm/cl_action_log_reader=>tyt_tp_rows.
    METHODS ts
      IMPORTING iv_time      TYPE tstamp
      RETURNING VALUE(rv_ts) TYPE timestampl.
    METHODS run_inside_action FOR TESTING.
    METHODS run_after_action_is_outside FOR TESTING.
    METHODS later_action_takes_reimport FOR TESTING.
    METHODS run_before_any_action_ignored FOR TESTING.
    METHODS export_and_import_split FOR TESTING.
    METHODS run_keeps_monitor_key FOR TESTING.
ENDCLASS.

CLASS ltc_attribution IMPLEMENTATION.
  METHOD ts.
    rv_ts = iv_time.
  ENDMETHOD.

  METHOD add_row.
    DATA ls_row TYPE /atrm/cl_action_log_reader=>ty_tp_row.
    ls_row-trkorr = iv_trkorr.
    ls_row-trtime = iv_time.
    ls_row-trstep = iv_step.
    ls_row-retcode = iv_retcode.
    ls_row-truser = 'TESTER'.
    ls_row-tpstat_key = iv_tpstat.
    APPEND ls_row TO ct_rows.
  ENDMETHOD.

  METHOD setup.
    DATA ls_link TYPE /atrm/cl_action_log_reader=>ty_link.
    " Install 1 of TRMK900010 on day 1, reinstall on day 5.
    ls_link-action_id = 'A1'.
    ls_link-trkorr = 'TRMK900010'.
    ls_link-started_at = ts( '20261101100000' ).
    ls_link-ended_at = ts( '20261101101000' ).
    APPEND ls_link TO mt_links.
    ls_link-action_id = 'A2'.
    ls_link-started_at = ts( '20261105100000' ).
    ls_link-ended_at = ts( '20261105101000' ).
    APPEND ls_link TO mt_links.
  ENDMETHOD.

  METHOD run_inside_action.
    DATA: lt_rows   TYPE /atrm/cl_action_log_reader=>tyt_tp_rows,
          lt_events TYPE /atrm/cl_action_log_reader=>tyt_events,
          ls_event  TYPE /atrm/cl_action_log_reader=>ty_event.
    add_row( EXPORTING iv_trkorr = 'TRMK900010' iv_time = '20261101100500' iv_step = 'H' CHANGING ct_rows = lt_rows ).
    add_row( EXPORTING iv_trkorr = 'TRMK900010' iv_time = '20261101100510' iv_step = 'A' iv_retcode = '0004' CHANGING ct_rows = lt_rows ).
    add_row( EXPORTING iv_trkorr = 'TRMK900010' iv_time = '20261101100520' iv_step = 'I' CHANGING ct_rows = lt_rows ).
    lt_events = /atrm/cl_action_log_reader=>attribute_tp_runs( it_tp_rows = lt_rows it_links = mt_links iv_time_zone = 'UTC' ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_events ) exp = 1 ).
    READ TABLE lt_events INTO ls_event INDEX 1.
    cl_abap_unit_assert=>assert_equals( act = ls_event-action_id exp = 'A1' ).
    cl_abap_unit_assert=>assert_equals( act = ls_event-event exp = 'IMPORT' ).
    cl_abap_unit_assert=>assert_equals( act = ls_event-retcode exp = '0004' ).
    cl_abap_unit_assert=>assert_equals( act = ls_event-outside exp = abap_false ).
    cl_abap_unit_assert=>assert_equals( act = ls_event-detail exp = 'HAI' ).
  ENDMETHOD.

  METHOD run_after_action_is_outside.
    DATA: lt_rows   TYPE /atrm/cl_action_log_reader=>tyt_tp_rows,
          lt_events TYPE /atrm/cl_action_log_reader=>tyt_events,
          ls_event  TYPE /atrm/cl_action_log_reader=>ty_event.
    add_row( EXPORTING iv_trkorr = 'TRMK900010' iv_time = '20261103090000' iv_step = 'I' CHANGING ct_rows = lt_rows ).
    lt_events = /atrm/cl_action_log_reader=>attribute_tp_runs( it_tp_rows = lt_rows it_links = mt_links iv_time_zone = 'UTC' ).
    READ TABLE lt_events INTO ls_event INDEX 1.
    cl_abap_unit_assert=>assert_equals( act = ls_event-action_id exp = 'A1' ).
    cl_abap_unit_assert=>assert_equals( act = ls_event-outside exp = abap_true ).
  ENDMETHOD.

  METHOD later_action_takes_reimport.
    DATA: lt_rows   TYPE /atrm/cl_action_log_reader=>tyt_tp_rows,
          lt_events TYPE /atrm/cl_action_log_reader=>tyt_events,
          ls_event  TYPE /atrm/cl_action_log_reader=>ty_event.
    add_row( EXPORTING iv_trkorr = 'TRMK900010' iv_time = '20261105100500' iv_step = 'I' CHANGING ct_rows = lt_rows ).
    lt_events = /atrm/cl_action_log_reader=>attribute_tp_runs( it_tp_rows = lt_rows it_links = mt_links iv_time_zone = 'UTC' ).
    READ TABLE lt_events INTO ls_event INDEX 1.
    cl_abap_unit_assert=>assert_equals( act = ls_event-action_id exp = 'A2' ).
    cl_abap_unit_assert=>assert_equals( act = ls_event-outside exp = abap_false ).
  ENDMETHOD.

  METHOD run_before_any_action_ignored.
    DATA: lt_rows   TYPE /atrm/cl_action_log_reader=>tyt_tp_rows,
          lt_events TYPE /atrm/cl_action_log_reader=>tyt_events.
    add_row( EXPORTING iv_trkorr = 'TRMK900010' iv_time = '20261001100000' iv_step = 'E' CHANGING ct_rows = lt_rows ).
    lt_events = /atrm/cl_action_log_reader=>attribute_tp_runs( it_tp_rows = lt_rows it_links = mt_links iv_time_zone = 'UTC' ).
    cl_abap_unit_assert=>assert_initial( lt_events ).
  ENDMETHOD.

  METHOD export_and_import_split.
    DATA: lt_rows   TYPE /atrm/cl_action_log_reader=>tyt_tp_rows,
          lt_events TYPE /atrm/cl_action_log_reader=>tyt_events,
          ls_event  TYPE /atrm/cl_action_log_reader=>ty_event.
    add_row( EXPORTING iv_trkorr = 'TRMK900010' iv_time = '20261101100100' iv_step = 'f' CHANGING ct_rows = lt_rows ).
    add_row( EXPORTING iv_trkorr = 'TRMK900010' iv_time = '20261101100105' iv_step = 'E' CHANGING ct_rows = lt_rows ).
    add_row( EXPORTING iv_trkorr = 'TRMK900010' iv_time = '20261101100200' iv_step = 'I' CHANGING ct_rows = lt_rows ).
    lt_events = /atrm/cl_action_log_reader=>attribute_tp_runs( it_tp_rows = lt_rows it_links = mt_links iv_time_zone = 'UTC' ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_events ) exp = 2 ).
    READ TABLE lt_events INTO ls_event INDEX 1.
    cl_abap_unit_assert=>assert_equals( act = ls_event-event exp = 'RELEASE' ).
    READ TABLE lt_events INTO ls_event INDEX 2.
    cl_abap_unit_assert=>assert_equals( act = ls_event-event exp = 'IMPORT' ).
  ENDMETHOD.

  METHOD run_keeps_monitor_key.
    DATA: lt_rows   TYPE /atrm/cl_action_log_reader=>tyt_tp_rows,
          lt_events TYPE /atrm/cl_action_log_reader=>tyt_events,
          ls_event  TYPE /atrm/cl_action_log_reader=>ty_event.
    " Only the main step of a run carries the key: the run takes it from any of its rows.
    add_row( EXPORTING iv_trkorr = 'TRMK900010' iv_time = '20261101100500' iv_step = 'H' CHANGING ct_rows = lt_rows ).
    add_row( EXPORTING iv_trkorr = 'TRMK900010' iv_time = '20261101100510' iv_step = 'I' iv_tpstat = '20261101100510123456' CHANGING ct_rows = lt_rows ).
    lt_events = /atrm/cl_action_log_reader=>attribute_tp_runs( it_tp_rows = lt_rows it_links = mt_links iv_time_zone = 'UTC' ).
    READ TABLE lt_events INTO ls_event INDEX 1.
    cl_abap_unit_assert=>assert_equals( act = ls_event-tpstat_key exp = '20261101100510123456' ).
  ENDMETHOD.
ENDCLASS.
