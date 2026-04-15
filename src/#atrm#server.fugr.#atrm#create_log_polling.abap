FUNCTION /atrm/create_log_polling.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(EVENT) TYPE  /ATRM/POLLING_EVENT
*"  EXPORTING
*"     VALUE(ID) TYPE  /ATRM/POLLING_ID
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
      go_log = /atrm/cl_log_polling=>create( event ).
      id = go_log->id.
    CATCH /atrm/cx_exception INTO go_exc.
      PERFORM handle_exception.
  ENDTRY.




ENDFUNCTION.
