FUNCTION ztrm_release_tr.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_TRKORR) TYPE  TRKORR
*"     VALUE(IV_LOCK) TYPE  FLAG
*"  EXPORTING
*"     VALUE(ET_MESSAGES) TYPE  CTSGERRMSGS
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
      zcl_trm_transport=>release(
        EXPORTING
          iv_trkorr   = iv_trkorr
          iv_lock     = iv_lock
        IMPORTING
          et_messages = et_messages
      ).
    CATCH zcx_trm_exception INTO lo_exc.
      PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.
