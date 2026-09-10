FUNCTION /atrm/set_install_devc.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(PACKAGE) TYPE  /ATRM/PACKAGES OPTIONAL
*"     VALUE(PACKAGE_EXISTS) TYPE  FLAG OPTIONAL
*"  TABLES
*"      INSTALLDEVC STRUCTURE  /ATRM/INSTDEVC
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      ENQUEUE_ERROR
*"      DEQUEUE_ERROR
*"      GENERIC
*"----------------------------------------------------------------------
  DATA lo_utilities TYPE REF TO /atrm/cl_utilities.

  PERFORM check_auth.

  TRY.
    IF package IS SUPPLIED.
      CREATE OBJECT lo_utilities.
      lo_utilities->restore_install_metadata(
        EXPORTING
          package        = package
          package_exists = package_exists
          installdevc    = installdevc[]
      ).
    ELSE.
      /atrm/cl_utilities=>add_install_devclass(
        EXPORTING
          installdevc = installdevc[]
      ).
    ENDIF.
  CATCH /atrm/cx_exception INTO go_exc.
    PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.
