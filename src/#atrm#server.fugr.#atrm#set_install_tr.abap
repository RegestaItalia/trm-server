FUNCTION /atrm/set_install_tr.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(PACKAGE_NAME) TYPE  /ATRM/PACKAGE_NAME
*"     VALUE(PACKAGE_REGISTRY) TYPE  /ATRM/PACKAGE_REGISTRY
*"  TABLES
*"      INSTALLTR STRUCTURE  /ATRM/INSTALLTR OPTIONAL
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      ENQUEUE_ERROR
*"      DEQUEUE_ERROR
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
    /atrm/cl_utilities=>set_install_transports(
      EXPORTING
        package_name     = package_name
        package_registry = package_registry
        installtr        = installtr[]
    ).
  CATCH /atrm/cx_exception INTO go_exc.
    PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.
