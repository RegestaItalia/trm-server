FUNCTION /ATRM/DELETE_LOG_POLLING.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(ID) TYPE  /ATRM/POLLING_ID
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
      CREATE OBJECT go_log EXPORTING id = id.
      go_log->delete( ).
    CATCH /atrm/cx_exception INTO go_exc.
      PERFORM handle_exception.
  ENDTRY.




ENDFUNCTION.
