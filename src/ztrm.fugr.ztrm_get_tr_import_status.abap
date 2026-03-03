FUNCTION ZTRM_GET_TR_IMPORT_STATUS.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_TRKORR) TYPE  TRKORR
*"     VALUE(IV_SYSTEM) TYPE  TMSSYSNAM
*"  EXPORTING
*"     VALUE(ES_STATUS) TYPE  TPSTAT
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
      CREATE OBJECT lo_transport EXPORTING iv_trkorr = iv_trkorr.
      es_status = lo_transport->get_import_status(
        EXPORTING
          iv_system = iv_system
      ).
    CATCH zcx_trm_exception INTO lo_exc.
      PERFORM handle_exception.
  ENDTRY.



ENDFUNCTION.
