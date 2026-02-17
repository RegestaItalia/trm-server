FUNCTION ztrm_read_log_polling.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_ID) TYPE  ZTRM_POLLING_ID
*"  EXPORTING
*"     VALUE(EV_LOG) TYPE  ZTRM_POLLING_LAST_MSG
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
      CREATE OBJECT lo_log EXPORTING id = iv_id.
      ev_log = lo_log->last_message.
    CATCH zcx_trm_exception INTO lo_exc.
      PERFORM handle_exception.
  ENDTRY.




ENDFUNCTION.
