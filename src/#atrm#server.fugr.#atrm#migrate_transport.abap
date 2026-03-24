FUNCTION /ATRM/MIGRATE_TRANSPORT.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(TRKORR) TYPE  TRKORR
*"  EXPORTING
*"     VALUE(TRM_TRKORR) TYPE  /ATRM/TRKORR
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      SNRO_INTERVAL_CREATE
*"      SNRO_INTERVAL_NOT_FOUND
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
      CREATE OBJECT go_transport EXPORTING trkorr = trkorr.
      go_transport->migrate(
        IMPORTING
          trm_trkorr = trm_trkorr
      ).
    CATCH /atrm/cx_exception INTO go_exc.
      PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.
