FUNCTION /atrm/delete_install_devc.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      INSTALLDEVC STRUCTURE  /ATRM/INSTDEVC
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      ENQUEUE_ERROR
*"      DEQUEUE_ERROR
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
    /atrm/cl_utilities=>delete_install_devclass(
      EXPORTING
        installdevc = installdevc[]
    ).
  CATCH /atrm/cx_exception INTO go_exc.
    PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.
