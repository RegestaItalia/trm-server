FUNCTION /atrm/add_src_trkorr.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(TRKORR) TYPE  TRKORR
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      ENQUEUE_ERROR
*"      DEQUEUE_ERROR
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
    /ATRM/CL_UTILITIES=>add_source_trkorr(
      EXPORTING
        trkorr = trkorr
    ).
  CATCH /atrm/cx_exception INTO go_exc.
    PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.
