FUNCTION ztrm_delete_transport.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_TRKORR) TYPE  TRKORR
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
      zcl_trm_transport=>delete(
        EXPORTING
          iv_trkorr = iv_trkorr
      ).
    CATCH zcx_trm_exception INTO lo_exc.
      PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.
