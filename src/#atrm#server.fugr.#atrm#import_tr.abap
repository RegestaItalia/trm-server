FUNCTION /ATRM/IMPORT_TR.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(SYSTEM) TYPE  TMSSYSNAM
*"     VALUE(TRKORR) TYPE  TRKORR
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
    CREATE OBJECT go_transport EXPORTING trkorr = trkorr.
    go_transport->import(
      EXPORTING
        system = system
    ).
  CATCH /atrm/cx_exception INTO go_exc.
    PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.
