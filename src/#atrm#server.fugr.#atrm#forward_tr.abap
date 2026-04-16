FUNCTION /ATRM/FORWARD_TR.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(TRKORR) TYPE  TRKORR
*"     VALUE(TARGET) TYPE  TMSSYSNAM
*"     VALUE(SOURCE) TYPE  TMSSYSNAM
*"     VALUE(IMPORT_AGAIN) TYPE  FLAG DEFAULT 'X'
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
    CREATE OBJECT go_transport EXPORTING trkorr = trkorr.
    go_transport->forward(
      EXPORTING
        target       = target
        source       = source
        import_again = import_again
    ).
  CATCH /atrm/cx_exception INTO go_exc.
    PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.
