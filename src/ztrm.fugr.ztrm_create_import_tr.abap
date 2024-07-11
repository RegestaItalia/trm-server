FUNCTION ztrm_create_import_tr.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_TEXT) TYPE  AS4TEXT
*"     VALUE(IV_TARGET) TYPE  TR_TARGET
*"  EXPORTING
*"     VALUE(EV_TRKORR) TYPE  TRKORR
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INSERT_FAILED
*"      ENQUEUE_FAILED
*"----------------------------------------------------------------------
  PERFORM check_auth.

  DATA ls_header TYPE trwbo_request_header.
  CALL FUNCTION 'TR_INSERT_REQUEST_WITH_TASKS'
    EXPORTING
      iv_text           = iv_text
      iv_type           = 'K'
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
