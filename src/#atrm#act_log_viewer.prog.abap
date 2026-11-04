REPORT /atrm/act_log_viewer.

TYPE-POOLS icon.

TYPES: BEGIN OF ty_out,
         at       TYPE c LENGTH 19,
         username TYPE xubname,
         status   TYPE c LENGTH 40,
         role     TYPE /atrm/log_tr_role,
         target   TYPE tmssysnam,
         retcode  TYPE trretcode,
         source   TYPE c LENGTH 3,
         contents TYPE c LENGTH 20,
         monitor  TYPE c LENGTH 20,
         detail   TYPE c LENGTH 80,
       END OF ty_out.
TYPES tyt_out TYPE STANDARD TABLE OF ty_out WITH DEFAULT KEY.

" What a node's links open: the transport contents, the import monitor (of one tp run, when known).
TYPES: BEGIN OF ty_node_target,
         node_key   TYPE salv_de_node_key,
         trkorr     TYPE trkorr,
         exists     TYPE abap_bool,
         system     TYPE tmssysnam,
         tpstat_key TYPE trtptstamp,
       END OF ty_node_target.

CONSTANTS: gc_col_contents TYPE lvc_fname VALUE 'CONTENTS',
           gc_col_monitor  TYPE lvc_fname VALUE 'MONITOR'.

DATA: gs_action       TYPE /atrm/log_act,
      gv_date         TYPE sydatum,
      gt_out          TYPE tyt_out,
      go_tree         TYPE REF TO cl_salv_tree,
      gt_actions      TYPE /atrm/cl_action_log_reader=>tyt_actions,
      gt_transports   TYPE /atrm/cl_action_log_reader=>tyt_transports,
      gt_events       TYPE /atrm/cl_action_log_reader=>tyt_events,
      gt_node_targets TYPE HASHED TABLE OF ty_node_target WITH UNIQUE KEY node_key.

CLASS lcl_events DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS on_link_click FOR EVENT link_click OF cl_salv_events_tree
      IMPORTING columnname node_key.
    CLASS-METHODS on_double_click FOR EVENT double_click OF cl_salv_events_tree
      IMPORTING columnname node_key.
    CLASS-METHODS open
      IMPORTING iv_node_key   TYPE salv_de_node_key
                iv_columnname TYPE lvc_fname.
ENDCLASS.

SELECT-OPTIONS: s_sess FOR gs_action-session_id LOWER CASE,
                s_action FOR gs_action-action_name LOWER CASE,
                s_pkg FOR gs_action-package_name LOWER CASE,
                s_user FOR gs_action-created_by,
                s_date FOR gv_date.

START-OF-SELECTION.
  AUTHORITY-CHECK OBJECT 'ZTRM_AUTH' ID 'ACTVT' FIELD '16'.
  IF sy-subrc <> 0.
    MESSAGE 'Not authorized to view the TRM action log'(e01) TYPE 'E'.
  ENDIF.

  /atrm/cl_action_log_reader=>read(
    EXPORTING
      ir_session = s_sess[]
      ir_action = s_action[]
      ir_package = s_pkg[]
      ir_user = s_user[]
      ir_date = s_date[]
    IMPORTING
      et_actions = gt_actions
      et_transports = gt_transports
      et_events = gt_events ).
  IF gt_actions IS INITIAL.
    MESSAGE 'No TRM actions found'(e02) TYPE 'S' DISPLAY LIKE 'W'.
    RETURN.
  ENDIF.

  PERFORM display_tree.

CLASS lcl_events IMPLEMENTATION.
  METHOD on_link_click.
    open( iv_node_key = node_key iv_columnname = columnname ).
  ENDMETHOD.

  METHOD on_double_click.
    " A double-click on a transport row opens its contents.
    open( iv_node_key = node_key iv_columnname = gc_col_contents ).
  ENDMETHOD.

  METHOD open.
    DATA ls_target TYPE ty_node_target.
    READ TABLE gt_node_targets INTO ls_target WITH TABLE KEY node_key = iv_node_key.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    CASE iv_columnname.
      WHEN gc_col_contents.
        IF ls_target-exists = abap_false.
          MESSAGE s000(0k) WITH 'Transport'(m01) ls_target-trkorr 'no longer exists'(m02) DISPLAY LIKE 'W'.
          RETURN.
        ENDIF.
        CALL FUNCTION 'TR_DISPLAY_REQUEST'
          EXPORTING
            i_trkorr = ls_target-trkorr.
      WHEN gc_col_monitor.
        CALL FUNCTION 'TMS_UI_IMPORT_MONITOR'
          EXPORTING
            iv_system   = ls_target-system
            iv_tpstatid = ls_target-tpstat_key.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.

FORM format_ts USING iv_ts TYPE timestampl CHANGING cv_text TYPE ty_out-at.
  DATA: lv_ts       TYPE timestamp,
        lv_date     TYPE d,
        lv_time     TYPE t,
        lv_dtext    TYPE c LENGTH 10,
        lv_ttext    TYPE c LENGTH 8,
        lv_timezone TYPE sy-zonlo.
  CLEAR cv_text.
  IF iv_ts IS INITIAL.
    RETURN.
  ENDIF.
  lv_timezone = sy-zonlo.
  IF lv_timezone IS INITIAL.
    lv_timezone = 'UTC'.
  ENDIF.
  " Truncate: rounding fractions of a second up can produce second 60.
  lv_ts = floor( iv_ts ).
  CONVERT TIME STAMP lv_ts TIME ZONE lv_timezone INTO DATE lv_date TIME lv_time.
  WRITE lv_date TO lv_dtext.
  WRITE lv_time TO lv_ttext.
  CONCATENATE lv_dtext lv_ttext INTO cv_text SEPARATED BY space.
ENDFORM.

FORM status_text USING iv_status TYPE /atrm/log_status
                 CHANGING cv_text TYPE ty_out-status cv_icon TYPE salv_de_tree_image.
  CASE iv_status.
    WHEN 'S'.
      cv_text = 'Success'(s01).
      cv_icon = icon_led_green.
    WHEN 'F'.
      cv_text = 'Failed'(s02).
      cv_icon = icon_led_red.
    WHEN 'B'.
      cv_text = 'Rolled back'(s03).
      cv_icon = icon_system_undo.
    WHEN OTHERS.
      cv_text = 'Running or interrupted'(s04).
      cv_icon = icon_led_yellow.
  ENDCASE.
ENDFORM.

FORM event_text USING iv_event TYPE /atrm/log_event CHANGING cv_text TYPE ty_out-status.
  CASE iv_event.
    WHEN 'CREATE'.
      cv_text = 'Created'(v01).
    WHEN 'RELEASE'.
      cv_text = 'Released'(v02).
    WHEN 'FORWARD'.
      cv_text = 'Forwarded'(v03).
    WHEN 'IMPORT'.
      cv_text = 'Imported'(v04).
    WHEN 'DELETE'.
      cv_text = 'Deleted'(v05).
    WHEN 'DEQUEUE'.
      cv_text = 'Removed from import queue'(v06).
    WHEN 'RESTORE'.
      cv_text = 'Restored'(v07).
    WHEN OTHERS.
      cv_text = iv_event.
  ENDCASE.
ENDFORM.

FORM add_node USING iv_parent TYPE salv_de_node_key
                    is_out TYPE ty_out
                    iv_text TYPE clike
                    iv_icon TYPE salv_de_tree_image
              CHANGING cv_key TYPE salv_de_node_key.
  DATA: lo_nodes TYPE REF TO cl_salv_nodes,
        lo_node  TYPE REF TO cl_salv_node,
        lv_text  TYPE lvc_value,
        lx_error TYPE REF TO cx_salv_msg.
  lo_nodes = go_tree->get_nodes( ).
  lv_text = iv_text.
  TRY.
      IF iv_parent IS INITIAL.
        lo_node = lo_nodes->add_node(
          related_node = ''
          relationship = cl_gui_column_tree=>relat_last_child
          data_row = is_out
          text = lv_text
          collapsed_icon = iv_icon
          expanded_icon = iv_icon ).
      ELSE.
        lo_node = lo_nodes->add_node(
          related_node = iv_parent
          relationship = cl_gui_column_tree=>relat_last_child
          data_row = is_out
          text = lv_text
          collapsed_icon = iv_icon
          expanded_icon = iv_icon ).
      ENDIF.
      cv_key = lo_node->get_key( ).
    CATCH cx_salv_msg INTO lx_error.
      MESSAGE lx_error TYPE 'E'.
  ENDTRY.
ENDFORM.

" Makes the filled link columns of a node clickable and records what they open.
FORM add_links USING iv_node_key TYPE salv_de_node_key
                     is_out TYPE ty_out
                     is_target TYPE ty_node_target.
  DATA: lo_node   TYPE REF TO cl_salv_node,
        lo_item   TYPE REF TO cl_salv_item,
        ls_target TYPE ty_node_target,
        lv_icon   TYPE salv_de_tree_image.
  ls_target = is_target.
  ls_target-node_key = iv_node_key.
  INSERT ls_target INTO TABLE gt_node_targets.
  TRY.
      lo_node = go_tree->get_nodes( )->get_node( iv_node_key ).
      IF is_out-contents IS NOT INITIAL.
        lo_item = lo_node->get_item( gc_col_contents ).
        lo_item->set_type( if_salv_c_item_type=>link ).
        lv_icon = icon_display.
        lo_item->set_icon( lv_icon ).
      ENDIF.
      IF is_out-monitor IS NOT INITIAL.
        lo_item = lo_node->get_item( gc_col_monitor ).
        lo_item->set_type( if_salv_c_item_type=>link ).
        lv_icon = icon_history.
        lo_item->set_icon( lv_icon ).
      ENDIF.
    CATCH cx_salv_msg.                                  "#EC NO_HANDLER
  ENDTRY.
ENDFORM.

FORM add_action USING is_action TYPE /atrm/log_act iv_parent TYPE salv_de_node_key.
  DATA: ls_out       TYPE ty_out,
        ls_child     TYPE /atrm/log_act,
        ls_transport TYPE /atrm/cl_action_log_reader=>ty_transport,
        ls_event     TYPE /atrm/cl_action_log_reader=>ty_event,
        ls_target    TYPE ty_node_target,
        lv_text      TYPE string,
        lv_ended     TYPE ty_out-at,
        lv_icon      TYPE salv_de_tree_image,
        lv_action    TYPE salv_de_node_key,
        lv_trnode    TYPE salv_de_node_key,
        lv_evnode    TYPE salv_de_node_key.

  CLEAR ls_out.
  PERFORM format_ts USING is_action-started_at CHANGING ls_out-at.
  ls_out-username = is_action-created_by.
  PERFORM status_text USING is_action-status CHANGING ls_out-status lv_icon.
  IF is_action-ended_at IS NOT INITIAL.
    PERFORM format_ts USING is_action-ended_at CHANGING lv_ended.
    CONCATENATE 'Ended'(t01) lv_ended INTO ls_out-detail SEPARATED BY space.
  ENDIF.
  lv_text = is_action-action_name.
  IF is_action-package_name IS NOT INITIAL.
    CONCATENATE lv_text is_action-package_name INTO lv_text SEPARATED BY space.
  ENDIF.
  IF is_action-version IS NOT INITIAL.
    CONCATENATE lv_text is_action-version INTO lv_text SEPARATED BY space.
  ENDIF.
  PERFORM add_node USING iv_parent ls_out lv_text lv_icon CHANGING lv_action.

  LOOP AT gt_transports INTO ls_transport WHERE action_id = is_action-action_id.
    CLEAR ls_out.
    PERFORM format_ts USING ls_transport-linked_at CHANGING ls_out-at.
    ls_out-role = ls_transport-role.
    ls_out-detail = ls_transport-as4text.
    IF ls_transport-exists = abap_false.
      ls_out-status = 'No longer exists'(t02).
      lv_icon = icon_delete.
    ELSEIF ls_transport-in_queue = abap_true.
      ls_out-status = 'In import queue'(t03).
      lv_icon = icon_transport.
    ELSEIF ls_transport-trstatus = 'R' OR ls_transport-trstatus = 'N'.
      ls_out-status = 'Released'(v02).
      lv_icon = icon_transport.
    ELSE.
      ls_out-status = 'Modifiable'(t04).
      lv_icon = icon_transport.
    ENDIF.
    IF ls_transport-exists = abap_true.
      ls_out-contents = 'Contents'(l01).
    ENDIF.
    ls_out-monitor = 'Import monitor'(l02).
    lv_text = ls_transport-trkorr.
    PERFORM add_node USING lv_action ls_out lv_text lv_icon CHANGING lv_trnode.
    CLEAR ls_target.
    ls_target-trkorr = ls_transport-trkorr.
    ls_target-exists = ls_transport-exists.
    ls_target-system = sy-sysid.
    PERFORM add_links USING lv_trnode ls_out ls_target.

    LOOP AT gt_events INTO ls_event
      WHERE action_id = is_action-action_id AND trkorr = ls_transport-trkorr.
      CLEAR ls_out.
      PERFORM format_ts USING ls_event-event_at CHANGING ls_out-at.
      ls_out-username = ls_event-username.
      ls_out-target = ls_event-target.
      ls_out-retcode = ls_event-retcode.
      ls_out-source = ls_event-source.
      PERFORM event_text USING ls_event-event CHANGING ls_out-status.
      IF ls_event-outside = abap_true.
        ls_out-detail = 'After the TRM action ended'(t05).
        lv_icon = icon_warning.
      ELSEIF ls_event-source = /atrm/cl_action_log_reader=>c_source-sap.
        CONCATENATE 'tp steps'(t06) ls_event-detail INTO ls_out-detail SEPARATED BY space.
        lv_icon = icon_information.
      ELSE.
        lv_icon = icon_okay.
      ENDIF.
      " An import opens the import monitor, at its own tp run when SAP recorded it.
      IF ls_event-event = 'IMPORT' AND ls_event-target IS NOT INITIAL.
        ls_out-monitor = 'Import monitor'(l02).
      ENDIF.
      lv_text = ls_out-status.
      PERFORM add_node USING lv_trnode ls_out lv_text lv_icon CHANGING lv_evnode.
      IF ls_out-monitor IS NOT INITIAL.
        CLEAR ls_target.
        ls_target-trkorr = ls_transport-trkorr.
        ls_target-exists = ls_transport-exists.
        ls_target-system = ls_event-target.
        ls_target-tpstat_key = ls_event-tpstat_key.
        PERFORM add_links USING lv_evnode ls_out ls_target.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

  LOOP AT gt_actions INTO ls_child WHERE parent_action_id = is_action-action_id.
    PERFORM add_action USING ls_child lv_action.
  ENDLOOP.
ENDFORM.

FORM display_tree.
  TYPES: BEGIN OF ty_session,
           session_id TYPE /atrm/log_session_id,
           started_at TYPE timestampl,
           created_by TYPE xubname,
         END OF ty_session.
  DATA: lt_sessions  TYPE STANDARD TABLE OF ty_session,
        ls_session   TYPE ty_session,
        ls_action    TYPE /atrm/log_act,
        ls_out       TYPE ty_out,
        lv_icon      TYPE salv_de_tree_image,
        lv_text      TYPE string,
        lv_tree_text TYPE salv_de_tree_text,
        lv_root      TYPE salv_de_node_key,
        lv_node      TYPE salv_de_node_key,
        lo_settings  TYPE REF TO cl_salv_tree_settings,
        lo_columns   TYPE REF TO cl_salv_columns_tree,
        lo_events    TYPE REF TO cl_salv_events_tree,
        lx_error     TYPE REF TO cx_salv_error.

  " Sessions: earliest action defines time and user.
  LOOP AT gt_actions INTO ls_action.
    READ TABLE lt_sessions INTO ls_session WITH KEY session_id = ls_action-session_id.
    IF sy-subrc <> 0.
      ls_session-session_id = ls_action-session_id.
      ls_session-started_at = ls_action-started_at.
      ls_session-created_by = ls_action-created_by.
      APPEND ls_session TO lt_sessions.
    ENDIF.
  ENDLOOP.
  SORT lt_sessions BY started_at DESCENDING.

  CLEAR: gt_out, gt_node_targets.
  TRY.
      cl_salv_tree=>factory( IMPORTING r_salv_tree = go_tree CHANGING t_table = gt_out ).
    CATCH cx_salv_error INTO lx_error.
      MESSAGE lx_error TYPE 'E'.
  ENDTRY.

  lo_settings = go_tree->get_tree_settings( ).
  lv_tree_text = 'Session / action / transport / event'(h01).
  lo_settings->set_hierarchy_header( lv_tree_text ).
  lo_settings->set_hierarchy_size( 50 ).
  lv_tree_text = 'TRM action log'(h02).
  lo_settings->set_header( lv_tree_text ).

  " Widths are wider than the values: proportional fonts render wider than the character count.
  lo_columns = go_tree->get_columns( ).
  PERFORM set_column_texts USING lo_columns 'AT' 'Time'(c01) 'Time'(c01) 32.
  PERFORM set_column_texts USING lo_columns 'USERNAME' '' '' 14.
  PERFORM set_column_texts USING lo_columns 'STATUS' 'Status / event'(c02) 'Status'(c03) 28.
  PERFORM set_column_texts USING lo_columns 'ROLE' '' '' 11.
  PERFORM set_column_texts USING lo_columns 'TARGET' '' '' 11.
  PERFORM set_column_texts USING lo_columns 'RETCODE' '' '' 9.
  PERFORM set_column_texts USING lo_columns 'SOURCE' 'Source'(c04) 'Source'(c04) 10.
  PERFORM set_column_texts USING lo_columns gc_col_contents 'Contents'(l01) 'Contents'(l01) 16.
  PERFORM set_column_texts USING lo_columns gc_col_monitor 'Import monitor'(l02) 'Monitor'(c06) 22.
  PERFORM set_column_texts USING lo_columns 'DETAIL' 'Details'(c05) 'Details'(c05) 60.

  LOOP AT lt_sessions INTO ls_session.
    CLEAR ls_out.
    PERFORM format_ts USING ls_session-started_at CHANGING ls_out-at.
    ls_out-username = ls_session-created_by.
    lv_text = ls_session-session_id.
    lv_icon = icon_history.
    PERFORM add_node USING lv_root ls_out lv_text lv_icon CHANGING lv_node.
    LOOP AT gt_actions INTO ls_action WHERE session_id = ls_session-session_id.
      " Nested actions hang under their parent when it is in the selection.
      IF ls_action-parent_action_id IS NOT INITIAL.
        READ TABLE gt_actions TRANSPORTING NO FIELDS WITH KEY action_id = ls_action-parent_action_id.
        IF sy-subrc = 0.
          CONTINUE.
        ENDIF.
      ENDIF.
      PERFORM add_action USING ls_action lv_node.
    ENDLOOP.
  ENDLOOP.

  lo_events = go_tree->get_event( ).
  SET HANDLER lcl_events=>on_link_click FOR lo_events.
  SET HANDLER lcl_events=>on_double_click FOR lo_events.

  go_tree->get_functions( )->set_all( abap_true ).
  go_tree->display( ).
ENDFORM.

FORM set_column_texts USING io_columns TYPE REF TO cl_salv_columns_tree
                            iv_name TYPE lvc_fname
                            iv_long TYPE clike
                            iv_short TYPE clike
                            iv_length TYPE i.
  DATA: lo_column TYPE REF TO cl_salv_column,
        lv_long   TYPE scrtext_l,
        lv_medium TYPE scrtext_m,
        lv_short  TYPE scrtext_s,
        lv_length TYPE lvc_outlen.
  " SALV texts are typed: text symbols must be moved to their types first.
  lv_long = iv_long.
  lv_medium = iv_long.
  lv_short = iv_short.
  " Nodes are added after the columns are built: widths can't be optimized on the data.
  lv_length = iv_length.
  TRY.
      lo_column = io_columns->get_column( iv_name ).
      IF iv_long IS NOT INITIAL.
        lo_column->set_long_text( lv_long ).
        lo_column->set_medium_text( lv_medium ).
        lo_column->set_short_text( lv_short ).
      ENDIF.
      lo_column->set_output_length( lv_length ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.
ENDFORM.
