FUNCTION /atrm/import_tr_multiple.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(SYSTEM) TYPE  TMSSYSNAM
*"     VALUE(TRKORR) TYPE  /ATRM/TRKORR_T
*"     VALUE(TEST) TYPE  FLAG
*"  EXPORTING
*"     VALUE(TEST_RESULT) TYPE  STRING
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
      /atrm/cl_transport=>import_multiple(
        EXPORTING
          system     = system
          transports = trkorr
          test       = test
      ).
    CATCH /atrm/cx_exception INTO go_exc.
      PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.
