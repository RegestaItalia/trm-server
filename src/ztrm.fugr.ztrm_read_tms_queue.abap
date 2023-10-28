FUNCTION ZTRM_READ_TMS_QUEUE.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_TARGET) TYPE  TMSSYSNAM
*"  EXPORTING
*"     VALUE(ET_REQUESTS) TYPE  TMSIQREQS
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      READ_QUEUE_FAILED
*"----------------------------------------------------------------------
  CALL FUNCTION 'ZTRM_CHECK_AUTH'
    EXCEPTIONS
      trm_rfc_unauthorized = 1.
  IF sy-subrc EQ 1.
    RAISE trm_rfc_unauthorized.
  ENDIF.

  CALL FUNCTION 'TMS_UIQ_IQD_READ_QUEUE'
    EXPORTING
      iv_system         = iv_target
      iv_collect        = 'X'
      iv_read_shadow    = 'X'
    IMPORTING
      et_requests       = et_requests
    EXCEPTIONS
      read_queue_failed = 1.

  IF sy-subrc EQ 1.
    RAISE read_queue_failed.
  ENDIF.

ENDFUNCTION.
