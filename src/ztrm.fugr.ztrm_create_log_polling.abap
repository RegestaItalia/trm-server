FUNCTION ztrm_create_log_polling.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_EVENT) TYPE  ZTRM_POLLING_EVENT
*"  EXPORTING
*"     VALUE(EV_ID) TYPE  ZTRM_POLLING_ID
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
      lo_log = zcl_trm_log_polling=>create( iv_event ).
      ev_id = lo_log->id.
    CATCH zcx_trm_exception INTO lo_exc.
      PERFORM handle_exception.
  ENDTRY.




ENDFUNCTION.
