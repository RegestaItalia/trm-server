FUNCTION ztrm_read_tms_queue.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_TARGET) TYPE  TMSSYSNAM
*"  EXPORTING
*"     VALUE(ET_REQUESTS) TYPE  TMSIQREQS
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      READ_QUEUE_FAILED
*"      TMS_ALERT
*"----------------------------------------------------------------------
  DATA: ls_bufcnt TYPE tmsbufcnt,
        ls_alog  TYPE tmsalog.

  CALL FUNCTION 'ZTRM_CHECK_AUTH'
    EXCEPTIONS
      trm_rfc_unauthorized = 1.
  IF sy-subrc EQ 1.
    RAISE trm_rfc_unauthorized.
  ENDIF.

  " 03072024 avoid display alert
  sy-batch = 'X'.

  CALL FUNCTION 'TMS_UIQ_IQD_READ_QUEUE'
    EXPORTING
      iv_system         = iv_target
      iv_collect        = 'X'
      iv_read_shadow    = 'X'
    IMPORTING
      et_requests       = et_requests
      es_bufcnt         = ls_bufcnt
    EXCEPTIONS
      read_queue_failed = 1.

  IF sy-subrc EQ 1.
    RAISE read_queue_failed.
  ENDIF.

  IF ls_bufcnt-alertid IS NOT INITIAL.
    CALL FUNCTION 'TMS_ALT_ANALYSE_ALERT'
      EXPORTING
        iv_alert_id   = ls_bufcnt-alertid
        iv_no_display = 'X'
      IMPORTING
        es_alog       = ls_alog
      EXCEPTIONS
        alert = 1
        error_message = 2
        OTHERS = 3.
     IF ls_alog-msgty EQ 'E' OR ls_alog-msgty EQ 'A'.
       MESSAGE ID ls_alog-msgid
       TYPE ls_alog-msgty
       NUMBER ls_alog-msgno
       WITH ls_alog-msgv1 ls_alog-msgv2 ls_alog-msgv3 ls_alog-msgv4
       RAISING tms_alert.
     ENDIF.
  ENDIF.

ENDFUNCTION.
