FUNCTION ztrm_dequeue_tr.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_TRKORR) TYPE  TRKORR
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"----------------------------------------------------------------------
  PERFORM check_auth.

  CALL FUNCTION 'DEQUEUE_E_TRKORR'
    EXPORTING
      trkorr = iv_trkorr.

ENDFUNCTION.
