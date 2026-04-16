FUNCTION /atrm/add_namespace.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(NAMESPACE) TYPE  NAMESPACE
*"     VALUE(REPLICENSE) TYPE  TRNLICENSE
*"  TABLES
*"      TEXTS STRUCTURE  TRNSPACETT
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
      /atrm/cl_utilities=>add_namespace(
        EXPORTING
          namespace  = namespace
          replicense = replicense
          texts      = texts[]
      ).
    CATCH /atrm/cx_exception INTO go_exc.
      PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.
