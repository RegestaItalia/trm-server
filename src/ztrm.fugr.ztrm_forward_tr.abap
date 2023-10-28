FUNCTION ZTRM_FORWARD_TR.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_TRKORR) TYPE  TRKORR
*"     VALUE(IV_TARGET) TYPE  TMSSYSNAM
*"     VALUE(IV_SOURCE) TYPE  TMSSYSNAM
*"     VALUE(IV_IMPORT_AGAIN) TYPE  FLAG DEFAULT 'X'
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

  CALL FUNCTION 'TMS_MGR_FORWARD_TR_REQUEST'
    EXPORTING
      iv_request                 = iv_trkorr
      iv_target                  = iv_target
      iv_source                  = iv_source
      iv_import_again            = iv_import_again
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
