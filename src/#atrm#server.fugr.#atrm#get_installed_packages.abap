FUNCTION /atrm/get_installed_packages.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(PACKAGES) TYPE  XSTRING
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
      DATA trm_packages TYPE /ATRM/CL_CORE=>tyt_trm_package.
      trm_packages = /ATRM/CL_SINGLETON=>get( )->get_installed_packages( ).
      CALL TRANSFORMATION id
      SOURCE packages = trm_packages
      RESULT XML packages.
    CATCH /atrm/cx_exception INTO go_exc.
      PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.
