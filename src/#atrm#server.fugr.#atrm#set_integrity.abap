FUNCTION /ATRM/SET_INTEGRITY.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(INTEGRITY) TYPE  /ATRM/INTEGRITY
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      ENQUEUE_ERROR
*"      DEQUEUE_ERROR
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
    /ATRM/CL_UTILITIES=>add_package_integrity(
      EXPORTING
        integrity = integrity
    ).
  CATCH /atrm/cx_exception INTO go_exc.
    PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.
