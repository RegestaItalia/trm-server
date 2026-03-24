FUNCTION /ATRM/SET_TRANSPORT_DOC.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(TRKORR) TYPE  TRKORR
*"  TABLES
*"      DOC STRUCTURE  TLINE
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      ENQUEUE_ERROR
*"      DEQUEUE_ERROR
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
    CREATE OBJECT go_transport EXPORTING trkorr = trkorr.
    go_transport->set_documentation(
      EXPORTING
        doc    = doc[]
    ).
  CATCH /atrm/cx_exception INTO go_exc.
    PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.
