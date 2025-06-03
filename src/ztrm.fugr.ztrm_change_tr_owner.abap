FUNCTION ztrm_change_tr_owner.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_TRKORR) TYPE  TRKORR
*"     VALUE(IV_NEW_OWNER) TYPE  TR_AS4USER
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
    CREATE OBJECT lo_transport EXPORTING iv_trkorr = iv_trkorr.
    lo_transport->set_owner(
      EXPORTING
        iv_user = iv_new_owner
    ).
  CATCH zcx_trm_exception INTO lo_exc.
    PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.
