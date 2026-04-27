FUNCTION /atrm/get_installed_packages.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(PACKAGE_NAME) TYPE  /ATRM/PACKAGE_NAME OPTIONAL
*"     VALUE(PACKAGE_REGISTRY) TYPE  /ATRM/PACKAGE_REGISTRY OPTIONAL
*"  EXPORTING
*"     VALUE(PACKAGES) TYPE  /ATRM/PACKAGES_T
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
      IF package_name IS INITIAL.
        packages = /atrm/cl_singleton=>get( )->get_installed_packages( ).
      ELSE.
        packages = /atrm/cl_core=>get_installed_packages(
          package_name     = package_name
          package_registry = package_registry
        ).
      ENDIF.
    CATCH /atrm/cx_exception INTO go_exc.
      PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.
