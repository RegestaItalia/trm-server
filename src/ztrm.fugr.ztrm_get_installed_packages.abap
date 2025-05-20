FUNCTION ztrm_get_installed_packages.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(EV_PACKAGES) TYPE  XSTRING
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
      DATA lt_packages TYPE zcl_trm_core=>tyt_trm_package.
      lt_packages = zcl_trm_core=>get_installed_packages( ).
      CALL TRANSFORMATION id
      SOURCE packages = lt_packages
      RESULT XML ev_packages.
    CATCH zcx_trm_exception INTO lo_exc.
      PERFORM handle_exception.
  ENDTRY.
ENDFUNCTION.
