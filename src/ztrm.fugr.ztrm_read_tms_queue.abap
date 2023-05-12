FUNCTION ZTRM_READ_TMS_QUEUE.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_TARGET) TYPE  TMSSYSNAM
*"  EXPORTING
*"     VALUE(ET_REQUESTS) TYPE  TMSIQREQS
*"  EXCEPTIONS
*"      READ_QUEUE_FAILED
*"----------------------------------------------------------------------

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
