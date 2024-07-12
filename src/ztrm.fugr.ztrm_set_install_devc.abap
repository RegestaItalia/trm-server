FUNCTION ztrm_set_install_devc.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      IT_INSTALLDEVC STRUCTURE  ZTRM_INSTALLDEVC
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      ENQUEUE_ERROR
*"      DEQUEUE_ERROR
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
      zcl_trm_utility=>add_install_devclass(
        EXPORTING
          it_installdevc = it_installdevc[]
      ).
    CATCH zcx_trm_exception INTO lo_exc.
      PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.
