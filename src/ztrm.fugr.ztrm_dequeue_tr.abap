FUNCTION ZTRM_DEQUEUE_TR.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_TRKORR) TYPE  TRKORR
*"----------------------------------------------------------------------
  CALL FUNCTION 'DEQUEUE_E_TRKORR'
    EXPORTING
      trkorr = iv_trkorr.

ENDFUNCTION.
