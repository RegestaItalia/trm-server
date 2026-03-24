FUNCTION /ATRM/GET_R3TRANS_INFO.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(LOG) TYPE  STRING
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      R3TRANS_CMD_ERROR
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
      log = /ATRM/CL_UTILITIES=>get_r3trans_info( ).
    CATCH /atrm/cx_exception INTO go_exc.
      PERFORM handle_exception.
  ENDTRY.


ENDFUNCTION.
