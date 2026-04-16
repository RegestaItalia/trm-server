FUNCTION /ATRM/GET_TR_IMPORT_STATUS.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(TRKORR) TYPE  TRKORR
*"     VALUE(SYSTEM) TYPE  TMSSYSNAM
*"  EXPORTING
*"     VALUE(STATUS) TYPE  TPSTAT
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
      CREATE OBJECT go_transport EXPORTING trkorr = trkorr.
      status = go_transport->get_import_status(
        EXPORTING
          system = system
      ).
    CATCH /atrm/cx_exception INTO go_exc.
      PERFORM handle_exception.
  ENDTRY.



ENDFUNCTION.
