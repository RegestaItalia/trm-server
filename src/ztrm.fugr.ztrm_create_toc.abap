FUNCTION ZTRM_CREATE_TOC.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_TEXT) TYPE  AS4TEXT
*"     VALUE(IV_TARGET) TYPE  TR_TARGET
*"  EXPORTING
*"     VALUE(EV_TRKORR) TYPE  TRKORR
*"  EXCEPTIONS
*"      INSERT_FAILED
*"      ENQUEUE_FAILED
*"----------------------------------------------------------------------
  DATA ls_header TYPE trwbo_request_header.
  CALL FUNCTION 'TR_INSERT_REQUEST_WITH_TASKS'
    EXPORTING
      iv_text           = iv_text
      iv_type           = 'T'
      iv_target         = iv_target
    IMPORTING
      es_request_header = ls_header
    EXCEPTIONS
      insert_failed     = 1
      enqueue_failed    = 2.

  IF sy-subrc EQ 0.
    ev_trkorr = ls_header-trkorr.
  ELSEIF sy-subrc EQ 1.
    RAISE insert_failed.
  ELSEIF sy-subrc EQ 2.
    RAISE enqueue_failed.
  ENDIF.

ENDFUNCTION.
