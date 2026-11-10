CLASS /atrm/cl_action_log_reader DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES tyt_actions TYPE STANDARD TABLE OF /atrm/log_act WITH DEFAULT KEY.
    TYPES tyr_session TYPE RANGE OF /atrm/log_session_id.
    TYPES tyr_action TYPE RANGE OF /atrm/log_action_name.
    TYPES tyr_package TYPE RANGE OF /atrm/package_name.
    TYPES tyr_user TYPE RANGE OF xubname.
    TYPES tyr_date TYPE RANGE OF sydatum.
    TYPES tyr_trkorr TYPE RANGE OF trkorr.
    TYPES:
      BEGIN OF ty_transport,
        action_id TYPE /atrm/log_action_id,
        trkorr    TYPE trkorr,
        role      TYPE /atrm/log_tr_role,
        linked_at TYPE timestampl,
        exists    TYPE abap_bool,
        trstatus  TYPE trstatus,
        as4text   TYPE as4text,
        in_queue  TYPE abap_bool,
      END OF ty_transport.
    TYPES tyt_transports TYPE STANDARD TABLE OF ty_transport WITH DEFAULT KEY.
    TYPES ty_source TYPE c LENGTH 3.
    TYPES:
      BEGIN OF ty_event,
        action_id  TYPE /atrm/log_action_id,
        trkorr     TYPE trkorr,
        event_at   TYPE timestampl,
        event      TYPE /atrm/log_event,
        source     TYPE ty_source,
        outside    TYPE abap_bool,
        target     TYPE tmssysnam,
        retcode    TYPE trretcode,
        username   TYPE xubname,
        detail     TYPE string,
        " tp run of a SAP event (import monitor entry)
        tpstat_key TYPE trtptstamp,
      END OF ty_event.
    TYPES tyt_events TYPE STANDARD TABLE OF ty_event WITH DEFAULT KEY.
    TYPES:
      BEGIN OF ty_tp_row,
        trkorr     TYPE trkorr,
        trtime     TYPE tstamp,
        trstep     TYPE trtpstep,
        retcode    TYPE trretcode,
        truser     TYPE as4user,
        tpstat_key TYPE trtptstamp,
      END OF ty_tp_row.
    TYPES tyt_tp_rows TYPE STANDARD TABLE OF ty_tp_row WITH DEFAULT KEY.
    TYPES:
      BEGIN OF ty_link,
        action_id  TYPE /atrm/log_action_id,
        trkorr     TYPE trkorr,
        started_at TYPE timestampl,
        ended_at   TYPE timestampl,
      END OF ty_link.
    TYPES tyt_links TYPE STANDARD TABLE OF ty_link WITH DEFAULT KEY.

    CONSTANTS:
      BEGIN OF c_source,
        trm TYPE ty_source VALUE 'TRM',
        sap TYPE ty_source VALUE 'SAP',
      END OF c_source.
    " Rows of one tp run that are further apart than this are separate runs.
    CONSTANTS c_run_gap_seconds TYPE i VALUE 900.

    CLASS-METHODS read
      IMPORTING ir_session    TYPE tyr_session OPTIONAL
                ir_action     TYPE tyr_action OPTIONAL
                ir_package    TYPE tyr_package OPTIONAL
                ir_user       TYPE tyr_user OPTIONAL
                ir_date       TYPE tyr_date OPTIONAL
      EXPORTING et_actions    TYPE tyt_actions
                et_transports TYPE tyt_transports
                et_events     TYPE tyt_events.
    " Groups TPALOG rows into tp runs and assigns each run to the latest
    " linked action that started at or before it.
    CLASS-METHODS attribute_tp_runs
      IMPORTING it_tp_rows      TYPE tyt_tp_rows
                it_links        TYPE tyt_links
                iv_time_zone    TYPE tznzone
      RETURNING VALUE(rt_events) TYPE tyt_events.
    " Assigns a moment to the latest linked action that started at or before it.
    CLASS-METHODS find_action
      IMPORTING it_links        TYPE tyt_links
                iv_trkorr       TYPE trkorr
                iv_at           TYPE timestampl
      EXPORTING ev_action_id    TYPE /atrm/log_action_id
                ev_outside      TYPE abap_bool.
    CLASS-METHODS to_timestamp
      IMPORTING iv_date      TYPE sydatum
                iv_time      TYPE syuzeit
                iv_time_zone TYPE tznzone
      RETURNING VALUE(rv_ts) TYPE timestampl.
    CLASS-METHODS is_export_step
      IMPORTING iv_step       TYPE trtpstep
      RETURNING VALUE(rv_yes) TYPE abap_bool.
  PRIVATE SECTION.
    CLASS-METHODS get_time_zone
      RETURNING VALUE(rv_zone) TYPE tznzone.
ENDCLASS.



CLASS /atrm/cl_action_log_reader IMPLEMENTATION.

  METHOD get_time_zone.
    SELECT SINGLE tzonesys FROM ttzcu INTO rv_zone.
    IF rv_zone IS INITIAL.
      rv_zone = 'UTC'.
    ENDIF.
  ENDMETHOD.

  METHOD to_timestamp.
    DATA lv_ts TYPE timestamp.
    CONVERT DATE iv_date TIME iv_time INTO TIME STAMP lv_ts TIME ZONE iv_time_zone.
    rv_ts = lv_ts.
  ENDMETHOD.

  METHOD is_export_step.
    IF iv_step = 'E' OR iv_step = 'e' OR iv_step = 'f'.
      rv_yes = abap_true.
    ELSE.
      rv_yes = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD find_action.
    DATA: ls_link TYPE ty_link,
          ls_best TYPE ty_link.

    CLEAR: ev_action_id, ev_outside.
    LOOP AT it_links INTO ls_link WHERE trkorr = iv_trkorr.
      IF ls_link-started_at > iv_at.
        CONTINUE.
      ENDIF.
      IF ls_best-action_id IS INITIAL OR ls_link-started_at > ls_best-started_at.
        ls_best = ls_link.
      ENDIF.
    ENDLOOP.
    IF ls_best-action_id IS INITIAL.
      RETURN.
    ENDIF.
    ev_action_id = ls_best-action_id.
    IF ls_best-ended_at IS NOT INITIAL AND iv_at > ls_best-ended_at.
      ev_outside = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD attribute_tp_runs.
    DATA: lt_rows      TYPE tyt_tp_rows,
          ls_row       TYPE ty_tp_row,
          ls_run       TYPE ty_event,
          lv_run_open  TYPE abap_bool,
          lv_last_ts   TYPE timestampl,
          lv_row_ts    TYPE timestampl,
          lv_last_trk  TYPE trkorr,
          lv_last_exp  TYPE abap_bool,
          lv_row_exp   TYPE abap_bool,
          lv_gap       TYPE tzntstmpl,
          lv_date      TYPE sydatum,
          lv_time      TYPE syuzeit,
          lv_ts_c      TYPE c LENGTH 14,
          lv_action_id TYPE /atrm/log_action_id,
          lv_outside   TYPE abap_bool.

    lt_rows = it_tp_rows.
    SORT lt_rows BY trkorr trtime trstep.

    LOOP AT lt_rows INTO ls_row.
      lv_ts_c = ls_row-trtime.
      lv_date = lv_ts_c(8).
      lv_time = lv_ts_c+8(6).
      lv_row_ts = to_timestamp( iv_date = lv_date iv_time = lv_time iv_time_zone = iv_time_zone ).
      lv_row_exp = is_export_step( ls_row-trstep ).

      IF lv_run_open = abap_true.
        lv_gap = cl_abap_tstmp=>subtract( tstmp1 = lv_row_ts tstmp2 = lv_last_ts ).
        IF ls_row-trkorr <> lv_last_trk OR lv_row_exp <> lv_last_exp OR lv_gap > c_run_gap_seconds.
          find_action( EXPORTING it_links = it_links iv_trkorr = ls_run-trkorr iv_at = ls_run-event_at
                       IMPORTING ev_action_id = lv_action_id ev_outside = lv_outside ).
          IF lv_action_id IS NOT INITIAL.
            ls_run-action_id = lv_action_id.
            ls_run-outside = lv_outside.
            APPEND ls_run TO rt_events.
          ENDIF.
          lv_run_open = abap_false.
        ENDIF.
      ENDIF.

      IF lv_run_open = abap_false.
        CLEAR ls_run.
        ls_run-trkorr = ls_row-trkorr.
        ls_run-event_at = lv_row_ts.
        ls_run-source = c_source-sap.
        ls_run-target = sy-sysid.
        IF lv_row_exp = abap_true.
          ls_run-event = 'RELEASE'.
        ELSE.
          ls_run-event = 'IMPORT'.
        ENDIF.
        lv_run_open = abap_true.
      ENDIF.

      IF ls_row-retcode > ls_run-retcode.
        ls_run-retcode = ls_row-retcode.
      ENDIF.
      IF ls_run-username IS INITIAL.
        ls_run-username = ls_row-truser.
      ENDIF.
      " Only some steps carry the run key of the import monitor.
      IF ls_run-tpstat_key IS INITIAL.
        ls_run-tpstat_key = ls_row-tpstat_key.
      ENDIF.
      IF ls_run-detail IS INITIAL.
        ls_run-detail = ls_row-trstep.
      ELSE.
        CONCATENATE ls_run-detail ls_row-trstep INTO ls_run-detail.
      ENDIF.
      lv_last_ts = lv_row_ts.
      lv_last_trk = ls_row-trkorr.
      lv_last_exp = lv_row_exp.
    ENDLOOP.

    IF lv_run_open = abap_true.
      find_action( EXPORTING it_links = it_links iv_trkorr = ls_run-trkorr iv_at = ls_run-event_at
                   IMPORTING ev_action_id = lv_action_id ev_outside = lv_outside ).
      IF lv_action_id IS NOT INITIAL.
        ls_run-action_id = lv_action_id.
        ls_run-outside = lv_outside.
        APPEND ls_run TO rt_events.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD read.
    TYPES: BEGIN OF ty_e070,
             trkorr   TYPE trkorr,
             trstatus TYPE trstatus,
           END OF ty_e070.
    TYPES: BEGIN OF ty_e07t,
             trkorr  TYPE trkorr,
             as4text TYPE as4text,
           END OF ty_e07t.
    DATA: lt_actions   TYPE tyt_actions,
          ls_action    TYPE /atrm/log_act,
          lt_all       TYPE tyt_actions,
          lt_trs       TYPE STANDARD TABLE OF /atrm/log_tr,
          ls_tr        TYPE /atrm/log_tr,
          lt_all_trs   TYPE STANDARD TABLE OF /atrm/log_tr,
          lt_evts      TYPE STANDARD TABLE OF /atrm/log_evt,
          ls_evt       TYPE /atrm/log_evt,
          ls_event     TYPE ty_event,
          lt_sap       TYPE tyt_events,
          ls_transport TYPE ty_transport,
          lt_links     TYPE tyt_links,
          ls_link      TYPE ty_link,
          lr_trkorr    TYPE tyr_trkorr,
          ls_trkorr    LIKE LINE OF lr_trkorr,
          lt_tp        TYPE tyt_tp_rows,
          lt_e070      TYPE STANDARD TABLE OF ty_e070,
          ls_e070      TYPE ty_e070,
          lt_e07t      TYPE STANDARD TABLE OF ty_e07t,
          ls_e07t      TYPE ty_e07t,
          lt_queue     TYPE STANDARD TABLE OF trkorr,
          lv_zone      TYPE tznzone,
          lv_date      TYPE sydatum,
          lv_time      TYPE syuzeit,
          lv_ts        TYPE timestamp.
    FIELD-SYMBOLS <ls_action> TYPE /atrm/log_act.

    CLEAR: et_actions, et_transports, et_events.
    lv_zone = get_time_zone( ).

    SELECT * FROM /atrm/log_act INTO TABLE lt_actions
      WHERE session_id IN ir_session
        AND action_name IN ir_action
        AND package_name IN ir_package
        AND created_by IN ir_user.
    IF ir_date IS NOT INITIAL.
      LOOP AT lt_actions INTO ls_action.
        " Truncate: rounding fractions of a second up can produce second 60.
        lv_ts = floor( ls_action-started_at ).
        CONVERT TIME STAMP lv_ts TIME ZONE sy-zonlo INTO DATE lv_date TIME lv_time.
        IF lv_date NOT IN ir_date.
          DELETE lt_actions.
        ENDIF.
      ENDLOOP.
    ENDIF.
    IF lt_actions IS INITIAL.
      RETURN.
    ENDIF.
    SORT lt_actions BY started_at action_id.
    et_actions = lt_actions.

    SELECT * FROM /atrm/log_tr INTO TABLE lt_trs
      FOR ALL ENTRIES IN lt_actions
      WHERE action_id = lt_actions-action_id.
    SELECT * FROM /atrm/log_evt INTO TABLE lt_evts
      FOR ALL ENTRIES IN lt_actions
      WHERE action_id = lt_actions-action_id.

    LOOP AT lt_evts INTO ls_evt.
      CLEAR ls_event.
      ls_event-action_id = ls_evt-action_id.
      ls_event-trkorr = ls_evt-trkorr.
      ls_event-event_at = ls_evt-event_at.
      ls_event-event = ls_evt-event.
      ls_event-source = c_source-trm.
      ls_event-target = ls_evt-target.
      ls_event-retcode = ls_evt-retcode.
      ls_event-username = ls_evt-created_by.
      APPEND ls_event TO et_events.
    ENDLOOP.

    IF lt_trs IS INITIAL.
      RETURN.
    ENDIF.

    ls_trkorr-sign = 'I'.
    ls_trkorr-option = 'EQ'.
    LOOP AT lt_trs INTO ls_tr.
      ls_trkorr-low = ls_tr-trkorr.
      COLLECT ls_trkorr INTO lr_trkorr.
    ENDLOOP.

    " Every action that linked these transports, including actions outside
    " the selection, so later actions keep the events that belong to them.
    SELECT * FROM /atrm/log_tr INTO TABLE lt_all_trs
      WHERE trkorr IN lr_trkorr.
    IF lt_all_trs IS NOT INITIAL.
      SELECT * FROM /atrm/log_act INTO TABLE lt_all
        FOR ALL ENTRIES IN lt_all_trs
        WHERE action_id = lt_all_trs-action_id.
    ENDIF.
    SORT lt_all BY action_id.
    LOOP AT lt_all_trs INTO ls_tr.
      READ TABLE lt_all ASSIGNING <ls_action> WITH KEY action_id = ls_tr-action_id BINARY SEARCH.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      CLEAR ls_link.
      ls_link-action_id = ls_tr-action_id.
      ls_link-trkorr = ls_tr-trkorr.
      ls_link-started_at = <ls_action>-started_at.
      ls_link-ended_at = <ls_action>-ended_at.
      APPEND ls_link TO lt_links.
    ENDLOOP.

    SELECT trkorr trtime trstep retcode truser tpstat_key FROM tpalog INTO TABLE lt_tp
      WHERE trkorr IN lr_trkorr
        AND tarsystem = sy-sysid.
    lt_sap = attribute_tp_runs( it_tp_rows = lt_tp it_links = lt_links iv_time_zone = lv_zone ).
    LOOP AT lt_sap INTO ls_event.
      READ TABLE lt_actions TRANSPORTING NO FIELDS WITH KEY action_id = ls_event-action_id.
      IF sy-subrc = 0.
        APPEND ls_event TO et_events.
      ENDIF.
    ENDLOOP.

    SELECT trkorr trstatus FROM e070 INTO TABLE lt_e070
      WHERE trkorr IN lr_trkorr.
    SORT lt_e070 BY trkorr.
    SELECT trkorr as4text FROM e07t INTO TABLE lt_e07t
      WHERE trkorr IN lr_trkorr
        AND langu = sy-langu.
    IF lt_e07t IS INITIAL.
      SELECT trkorr as4text FROM e07t INTO TABLE lt_e07t
        WHERE trkorr IN lr_trkorr.
    ENDIF.
    SORT lt_e07t BY trkorr.
    SELECT trkorr FROM tmsbuffer INTO TABLE lt_queue
      WHERE sysnam = sy-sysid
        AND trkorr IN lr_trkorr.
    SORT lt_queue.

    LOOP AT lt_trs INTO ls_tr.
      CLEAR ls_transport.
      ls_transport-action_id = ls_tr-action_id.
      ls_transport-trkorr = ls_tr-trkorr.
      ls_transport-role = ls_tr-role.
      ls_transport-linked_at = ls_tr-linked_at.
      READ TABLE lt_e070 INTO ls_e070 WITH KEY trkorr = ls_tr-trkorr BINARY SEARCH.
      IF sy-subrc = 0.
        ls_transport-exists = abap_true.
        ls_transport-trstatus = ls_e070-trstatus.
      ENDIF.
      READ TABLE lt_e07t INTO ls_e07t WITH KEY trkorr = ls_tr-trkorr BINARY SEARCH.
      IF sy-subrc = 0.
        ls_transport-as4text = ls_e07t-as4text.
      ENDIF.
      READ TABLE lt_queue TRANSPORTING NO FIELDS WITH KEY table_line = ls_tr-trkorr BINARY SEARCH.
      IF sy-subrc = 0.
        ls_transport-in_queue = abap_true.
      ENDIF.
      APPEND ls_transport TO et_transports.
    ENDLOOP.
    SORT et_transports BY action_id linked_at trkorr.
    SORT et_events BY action_id trkorr event_at.
  ENDMETHOD.

ENDCLASS.
