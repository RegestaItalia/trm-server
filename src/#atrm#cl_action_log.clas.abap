CLASS /atrm/cl_action_log DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES tyt_events TYPE STANDARD TABLE OF /atrm/log_evt_in WITH DEFAULT KEY.
    CONSTANTS:
      BEGIN OF c_status,
        running     TYPE /atrm/log_status VALUE 'R',
        success     TYPE /atrm/log_status VALUE 'S',
        failed      TYPE /atrm/log_status VALUE 'F',
        rolled_back TYPE /atrm/log_status VALUE 'B',
      END OF c_status.
    CLASS-METHODS start_action
      IMPORTING iv_action_id        TYPE /atrm/log_action_id
                iv_session_id       TYPE /atrm/log_session_id
                iv_parent_action_id TYPE /atrm/log_action_id OPTIONAL
                iv_action_name      TYPE /atrm/log_action_name
                iv_package_name     TYPE /atrm/package_name OPTIONAL
                iv_package_registry TYPE /atrm/package_registry OPTIONAL
                iv_version          TYPE /atrm/log_version OPTIONAL
      RAISING   /atrm/cx_exception.
CLASS-METHODS end_action
      IMPORTING iv_action_id TYPE /atrm/log_action_id
                iv_status    TYPE /atrm/log_status
                iv_version   TYPE /atrm/log_version OPTIONAL
      RAISING   /atrm/cx_exception.
    CLASS-METHODS add_events
      IMPORTING iv_action_id TYPE /atrm/log_action_id
                it_events    TYPE tyt_events
      RAISING   /atrm/cx_exception.
  PRIVATE SECTION.
    CLASS-METHODS enqueue_action
      IMPORTING iv_action_id TYPE /atrm/log_action_id
      RAISING   /atrm/cx_exception.
    CLASS-METHODS dequeue_action
      IMPORTING iv_action_id TYPE /atrm/log_action_id.
ENDCLASS.



CLASS /atrm/cl_action_log IMPLEMENTATION.

  METHOD enqueue_action.
    DATA lv_varkey TYPE rstable-varkey.
    lv_varkey = iv_action_id.
    CALL FUNCTION 'ENQUEUE_E_TABLE'
      EXPORTING
        tabname        = '/ATRM/LOG_ACT'
        varkey         = lv_varkey
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
      /atrm/cx_exception=>raise(
        iv_reason = /atrm/cx_exception=>c_reason-enqueue_error
        iv_message = 'TRM action log entry is busy' ).
    ENDIF.
  ENDMETHOD.

  METHOD dequeue_action.
    DATA lv_varkey TYPE rstable-varkey.
    lv_varkey = iv_action_id.
    CALL FUNCTION 'DEQUEUE_E_TABLE'
      EXPORTING
        tabname = '/ATRM/LOG_ACT'
        varkey  = lv_varkey.
  ENDMETHOD.

  METHOD start_action.
    DATA: ls_action TYPE /atrm/log_act,
          lo_error  TYPE REF TO /atrm/cx_exception,
          lo_root   TYPE REF TO cx_root.

    IF iv_action_id IS INITIAL OR iv_session_id IS INITIAL OR iv_action_name IS INITIAL.
      /atrm/cx_exception=>raise(
        iv_reason = /atrm/cx_exception=>c_reason-invalid_input
        iv_message = 'Action ID, session ID and action name are required' ).
    ENDIF.

    enqueue_action( iv_action_id ).
    TRY.
        ls_action-action_id = iv_action_id.
        ls_action-session_id = iv_session_id.
        ls_action-parent_action_id = iv_parent_action_id.
        ls_action-action_name = iv_action_name.
        ls_action-package_name = iv_package_name.
        ls_action-package_registry = iv_package_registry.
        ls_action-version = iv_version.
        ls_action-status = c_status-running.
        ls_action-created_by = sy-uname.
        GET TIME STAMP FIELD ls_action-started_at.
        INSERT /atrm/log_act FROM ls_action.
        IF sy-subrc <> 0.
          /atrm/cx_exception=>raise(
            iv_reason = /atrm/cx_exception=>c_reason-insert_error
            iv_message = 'TRM action log entry already exists' ).
        ENDIF.
        COMMIT WORK AND WAIT.
      CATCH /atrm/cx_exception INTO lo_error.
        ROLLBACK WORK.
        dequeue_action( iv_action_id ).
        RAISE EXCEPTION lo_error.
      CATCH cx_root INTO lo_root.
        ROLLBACK WORK.
        dequeue_action( iv_action_id ).
        /atrm/cx_exception=>raise(
          iv_reason = /atrm/cx_exception=>c_reason-generic
          io_root = lo_root ).
    ENDTRY.
    dequeue_action( iv_action_id ).
  ENDMETHOD.

  METHOD end_action.
    DATA: lv_ended_at TYPE timestampl,
          lo_error    TYPE REF TO /atrm/cx_exception,
          lo_root     TYPE REF TO cx_root.

    IF iv_action_id IS INITIAL.
      /atrm/cx_exception=>raise(
        iv_reason = /atrm/cx_exception=>c_reason-invalid_input
        iv_message = 'Action ID is required' ).
    ENDIF.
    IF iv_status <> c_status-success AND iv_status <> c_status-failed AND iv_status <> c_status-rolled_back.
      /atrm/cx_exception=>raise(
        iv_reason = /atrm/cx_exception=>c_reason-invalid_input
        iv_message = 'Final action status must be S, F or B' ).
    ENDIF.

    enqueue_action( iv_action_id ).
    TRY.
        GET TIME STAMP FIELD lv_ended_at.
        " The version an action resolves (e.g. latest) is known only at its end.
        IF iv_version IS INITIAL.
          UPDATE /atrm/log_act SET status = iv_status ended_at = lv_ended_at
            WHERE action_id = iv_action_id.
        ELSE.
          UPDATE /atrm/log_act SET status = iv_status ended_at = lv_ended_at version = iv_version
            WHERE action_id = iv_action_id.
        ENDIF.
        IF sy-subrc <> 0.
          /atrm/cx_exception=>raise(
            iv_reason = /atrm/cx_exception=>c_reason-not_found
            iv_message = 'TRM action log entry not found' ).
        ENDIF.
        COMMIT WORK AND WAIT.
      CATCH /atrm/cx_exception INTO lo_error.
        ROLLBACK WORK.
        dequeue_action( iv_action_id ).
        RAISE EXCEPTION lo_error.
      CATCH cx_root INTO lo_root.
        ROLLBACK WORK.
        dequeue_action( iv_action_id ).
        /atrm/cx_exception=>raise(
          iv_reason = /atrm/cx_exception=>c_reason-generic
          io_root = lo_root ).
    ENDTRY.
    dequeue_action( iv_action_id ).
  ENDMETHOD.

  METHOD add_events.
    DATA: ls_event TYPE /atrm/log_evt_in,
          ls_link  TYPE /atrm/log_tr,
          ls_row   TYPE /atrm/log_evt,
          lv_seq   TYPE /atrm/log_seq,
          lv_now   TYPE timestampl,
          lo_error TYPE REF TO /atrm/cx_exception,
          lo_root  TYPE REF TO cx_root.

    IF iv_action_id IS INITIAL.
      /atrm/cx_exception=>raise(
        iv_reason = /atrm/cx_exception=>c_reason-invalid_input
        iv_message = 'Action ID is required' ).
    ENDIF.
    LOOP AT it_events INTO ls_event.
      IF ls_event-trkorr IS INITIAL OR ls_event-event IS INITIAL.
        /atrm/cx_exception=>raise(
          iv_reason = /atrm/cx_exception=>c_reason-invalid_input
          iv_message = 'Transport and event are required' ).
      ENDIF.
    ENDLOOP.
    IF it_events IS INITIAL.
      RETURN.
    ENDIF.

    enqueue_action( iv_action_id ).
    TRY.
        SELECT SINGLE action_id FROM /atrm/log_act INTO ls_link-action_id
          WHERE action_id = iv_action_id.
        IF sy-subrc <> 0.
          /atrm/cx_exception=>raise(
            iv_reason = /atrm/cx_exception=>c_reason-not_found
            iv_message = 'TRM action log entry not found' ).
        ENDIF.
        SELECT MAX( seq ) FROM /atrm/log_evt INTO lv_seq
          WHERE action_id = iv_action_id.
        GET TIME STAMP FIELD lv_now.

        LOOP AT it_events INTO ls_event.
          CLEAR ls_link.
          SELECT SINGLE * FROM /atrm/log_tr INTO ls_link
            WHERE action_id = iv_action_id
              AND trkorr = ls_event-trkorr.
          IF sy-subrc <> 0.
            ls_link-action_id = iv_action_id.
            ls_link-trkorr = ls_event-trkorr.
            ls_link-role = ls_event-role.
            ls_link-linked_at = lv_now.
            INSERT /atrm/log_tr FROM ls_link.
            IF sy-subrc <> 0.
              /atrm/cx_exception=>raise(
                iv_reason = /atrm/cx_exception=>c_reason-insert_error
                iv_message = 'Could not persist TRM action log transport' ).
            ENDIF.
          ELSEIF ls_link-role IS INITIAL AND ls_event-role IS NOT INITIAL.
            UPDATE /atrm/log_tr SET role = ls_event-role
              WHERE action_id = iv_action_id
                AND trkorr = ls_event-trkorr.
          ENDIF.

          lv_seq = lv_seq + 1.
          CLEAR ls_row.
          ls_row-action_id = iv_action_id.
          ls_row-seq = lv_seq.
          ls_row-trkorr = ls_event-trkorr.
          ls_row-event = ls_event-event.
          ls_row-target = ls_event-target.
          ls_row-retcode = ls_event-retcode.
          ls_row-event_at = lv_now.
          ls_row-created_by = sy-uname.
          INSERT /atrm/log_evt FROM ls_row.
          IF sy-subrc <> 0.
            /atrm/cx_exception=>raise(
              iv_reason = /atrm/cx_exception=>c_reason-insert_error
              iv_message = 'Could not persist TRM action log event' ).
          ENDIF.
        ENDLOOP.
        COMMIT WORK AND WAIT.
      CATCH /atrm/cx_exception INTO lo_error.
        ROLLBACK WORK.
        dequeue_action( iv_action_id ).
        RAISE EXCEPTION lo_error.
      CATCH cx_root INTO lo_root.
        ROLLBACK WORK.
        dequeue_action( iv_action_id ).
        /atrm/cx_exception=>raise(
          iv_reason = /atrm/cx_exception=>c_reason-generic
          io_root = lo_root ).
    ENDTRY.
    dequeue_action( iv_action_id ).
  ENDMETHOD.

ENDCLASS.
