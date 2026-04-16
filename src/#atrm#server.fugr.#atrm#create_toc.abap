FUNCTION /atrm/create_toc.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(TEXT) TYPE  AS4TEXT
*"     VALUE(TARGET) TYPE  TR_TARGET
*"  EXPORTING
*"     VALUE(TRKORR) TYPE  TRKORR
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
    /atrm/cl_transport=>create_transport_of_copies(
      EXPORTING
        text   = text
        target = target
      RECEIVING
        transport = go_transport
    ).
    trkorr = go_transport->get_trkorr( ).
  CATCH /atrm/cx_exception INTO go_exc.
    PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.
