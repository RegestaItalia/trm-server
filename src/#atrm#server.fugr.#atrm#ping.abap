FUNCTION /ATRM/PING.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(RETURN) TYPE  STRING
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"----------------------------------------------------------------------
  PERFORM check_auth.

  return = 'PONG'.


ENDFUNCTION.
