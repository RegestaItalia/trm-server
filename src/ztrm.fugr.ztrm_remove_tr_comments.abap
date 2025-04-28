FUNCTION ztrm_remove_tr_comments.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_TRKORR) TYPE  TRKORR
*"     VALUE(IV_OBJECT) TYPE  TROBJTYPE
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
      CREATE OBJECT lo_transport EXPORTING iv_trkorr = iv_trkorr.
      lo_transport->remove_comments( iv_object = iv_object ).
    CATCH zcx_trm_exception INTO lo_exc.
      PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.
