FUNCTION /ATRM/CREATE_PACKAGE.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(DATA) TYPE  SCOMPKDTLN
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      DYN_CALL_PARAM_NOT_FOUND
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
      /ATRM/CL_PACKAGE=>create(
        EXPORTING
          data = data
      ).
    CATCH /atrm/cx_exception INTO go_exc.
      PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.
