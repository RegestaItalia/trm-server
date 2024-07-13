FUNCTION ztrm_forward_tr.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_TRKORR) TYPE  TRKORR
*"     VALUE(IV_TARGET) TYPE  TMSSYSNAM
*"     VALUE(IV_SOURCE) TYPE  TMSSYSNAM
*"     VALUE(IV_IMPORT_AGAIN) TYPE  FLAG DEFAULT 'X'
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
    CREATE OBJECT lo_transport EXPORTING iv_trkorr = iv_trkorr.
    lo_transport->forward(
      EXPORTING
        iv_target       = iv_target
        iv_source       = iv_source
        iv_import_again = iv_import_again
    ).
  CATCH zcx_trm_exception INTO lo_exc.
    PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.
