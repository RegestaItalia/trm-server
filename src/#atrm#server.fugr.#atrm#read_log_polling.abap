FUNCTION /ATRM/READ_LOG_POLLING.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(ID) TYPE  /ATRM/POLLING_ID
*"  EXPORTING
*"     VALUE(LOG) TYPE  /ATRM/POLLING_LAST_MSG
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
      CREATE OBJECT go_log EXPORTING id = id.
      log = go_log->last_message.
    CATCH /atrm/cx_exception INTO go_exc.
      PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.
