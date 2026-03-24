FUNCTION /atrm/add_lang_tr.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(TRKORR) TYPE  TRKORR
*"  TABLES
*"      DEVCLASS STRUCTURE  LXE_TT_PACKG_LINE
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
      CREATE OBJECT go_transport EXPORTING trkorr = trkorr.
      go_transport->add_translations(
        EXPORTING
          devclass = devclass[]
      ).
    CATCH /atrm/cx_exception INTO go_exc.
      PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.
