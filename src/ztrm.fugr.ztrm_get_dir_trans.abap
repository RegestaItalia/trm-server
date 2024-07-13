FUNCTION ztrm_get_dir_trans.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(EV_DIR_TRANS) TYPE  PFEVALUE
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
    zcl_trm_utility=>get_dir_trans(
      IMPORTING
        ev_dir_trans = ev_dir_trans
    ).
  CATCH zcx_trm_exception INTO lo_exc.
    PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.
