FUNCTION ztrm_ping.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(EV_RETURN) TYPE  STRING
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"----------------------------------------------------------------------
  PERFORM check_auth.

  ev_return = 'PONG'.


ENDFUNCTION.
