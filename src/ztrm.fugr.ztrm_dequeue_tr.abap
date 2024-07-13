FUNCTION ztrm_dequeue_tr.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_TRKORR) TYPE  TRKORR
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      DEQUEUE_ERROR
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
    CREATE OBJECT lo_transport EXPORTING iv_trkorr = iv_trkorr.
    lo_transport->dequeue( ).
  CATCH zcx_trm_exception INTO lo_exc.
    PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.
