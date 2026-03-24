FUNCTION /atrm/create_import_tr.
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
    /ATRM/CL_TRANSPORT=>create_workbench(
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
