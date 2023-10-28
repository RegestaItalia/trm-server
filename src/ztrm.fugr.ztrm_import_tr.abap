FUNCTION ZTRM_IMPORT_TR.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_SYSTEM) TYPE  TMSSYSNAM
*"     VALUE(IV_TRKORR) TYPE  TRKORR
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      READ_CONFIG_FAILED
*"      TABLE_OF_REQUESTS_IS_EMPTY
*"      ERROR_MESSAGE
*"----------------------------------------------------------------------
  CALL FUNCTION 'ZTRM_CHECK_AUTH'
    EXCEPTIONS
      trm_rfc_unauthorized = 1.
  IF sy-subrc EQ 1.
    RAISE trm_rfc_unauthorized.
  ENDIF.

  CALL FUNCTION 'TMS_MGR_IMPORT_TR_REQUEST'
    EXPORTING
      iv_system                  = iv_system
      iv_request                 = iv_trkorr
      iv_ctc_active              = ' '
      iv_overtake                = 'X'
      iv_import_again            = 'X'
      iv_ignore_originality      = 'X'
      iv_ignore_repairs          = 'X'
      iv_ignore_transtype        = 'X'
      iv_ignore_tabletype        = 'X'
      iv_ignore_predec           = 'X'
      iv_ignore_cvers            = 'X'
      iv_test_import             = ' '
      iv_subset                  = 'X'
      iv_offline                 = 'X'
      iv_monitor                 = 'X'
      iv_verbose                 = ' '
    EXCEPTIONS
      read_config_failed         = 1
      table_of_requests_is_empty = 2
      error_message              = 3.

  IF sy-subrc EQ 1.
    RAISE read_config_failed.
  ELSEIF sy-subrc EQ 2.
    RAISE table_of_requests_is_empty.
  ELSEIF sy-subrc EQ 3.
    RAISE error_message.
  ENDIF.

ENDFUNCTION.
