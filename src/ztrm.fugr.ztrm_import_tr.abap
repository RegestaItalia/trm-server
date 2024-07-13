FUNCTION ztrm_import_tr.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_SYSTEM) TYPE  TMSSYSNAM
*"     VALUE(IV_TRKORR) TYPE  TRKORR
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
    CREATE OBJECT lo_transport EXPORTING iv_trkorr = iv_trkorr.
    lo_transport->import(
      EXPORTING
        iv_system = iv_system
    ).
  CATCH zcx_trm_exception INTO lo_exc.
    PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.
