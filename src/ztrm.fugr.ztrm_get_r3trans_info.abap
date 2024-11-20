FUNCTION ztrm_get_r3trans_info.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(EV_LOG) TYPE  STRING
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      R3TRANS_CMD_ERROR
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
      ev_log = zcl_trm_utility=>get_r3trans_info( ).
    CATCH zcx_trm_exception INTO lo_exc.
      PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.
