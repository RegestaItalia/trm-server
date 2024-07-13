FUNCTION ztrm_add_lang_tr.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_TRKORR) TYPE  TRKORR
*"  TABLES
*"      IT_DEVCLASS STRUCTURE  LXE_TT_PACKG_LINE
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
    CREATE OBJECT lo_transport EXPORTING iv_trkorr = iv_trkorr.
    lo_transport->add_translations(
      EXPORTING
        it_devclass = it_devclass[]
    ).
  CATCH zcx_trm_exception INTO lo_exc.
    PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.
