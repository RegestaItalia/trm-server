FUNCTION ztrm_tdevc_interface.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_DEVCLASS) TYPE  DEVCLASS
*"     VALUE(IV_PARENTCL) TYPE  DEVCLASS OPTIONAL
*"     VALUE(IV_RM_PARENTCL) TYPE  FLAG OPTIONAL
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
    CREATE OBJECT lo_package EXPORTING iv_devclass = iv_devclass.
    lo_package->interface(
      EXPORTING
        iv_parentcl    = iv_parentcl
        iv_rm_parentcl = iv_rm_parentcl
    ).
  CATCH zcx_trm_exception INTO lo_exc.
    PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.
