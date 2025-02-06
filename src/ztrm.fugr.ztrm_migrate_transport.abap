FUNCTION ztrm_migrate_transport.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_TRKORR) TYPE  TRKORR
*"  EXPORTING
*"     VALUE(EV_TRM_TRKORR) TYPE  ZTRM_TRKORR
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      SNRO_INTERVAL_NOT_FOUND
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
      CREATE OBJECT lo_transport EXPORTING iv_trkorr = iv_trkorr.
      lo_transport->migrate(
        IMPORTING
          ev_trm_trkorr = ev_trm_trkorr
      ).
    CATCH zcx_trm_exception INTO lo_exc.
      PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.
