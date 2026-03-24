FUNCTION /ATRM/TR_COPY.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(FROM) TYPE  TRKORR
*"     VALUE(TO) TYPE  TRKORR
*"     VALUE(DOC) TYPE  TRPARFLAG
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
    CREATE OBJECT go_transport EXPORTING trkorr = to.
    go_transport->copy(
      EXPORTING
        trkorr = from
        doc    = doc
    ).
  CATCH /atrm/cx_exception INTO go_exc.
    PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.
