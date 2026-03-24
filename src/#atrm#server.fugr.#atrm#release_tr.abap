FUNCTION /ATRM/RELEASE_TR.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(TRKORR) TYPE  TRKORR
*"     VALUE(LOCK) TYPE  FLAG
*"  EXPORTING
*"     VALUE(MESSAGES) TYPE  CTSGERRMSGS
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
    CREATE OBJECT go_transport EXPORTING trkorr = trkorr.
    go_transport->release(
      EXPORTING
        lock     = lock
      IMPORTING
        messages = messages[]
    ).
  CATCH /atrm/cx_exception INTO go_exc.
    PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.
