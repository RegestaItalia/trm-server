FUNCTION ztrm_create_import_tr.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_TEXT) TYPE  AS4TEXT
*"     VALUE(IV_TARGET) TYPE  TR_TARGET
*"  EXPORTING
*"     VALUE(EV_TRKORR) TYPE  TRKORR
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
    zcl_trm_transport=>create_workbench(
      EXPORTING
        iv_text   = iv_text
        iv_target = iv_target
      RECEIVING
        ro_transport = lo_transport
    ).
    ev_trkorr = lo_transport->get_trkorr( ).
  CATCH zcx_trm_exception INTO lo_exc.
    PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.
