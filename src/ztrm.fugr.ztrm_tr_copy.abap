FUNCTION ztrm_tr_copy.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_FROM) TYPE  TRKORR
*"     VALUE(IV_TO) TYPE  TRKORR
*"     VALUE(IV_DOC) TYPE  TRPARFLAG
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
    CREATE OBJECT lo_transport EXPORTING iv_trkorr = iv_to.
    lo_transport->copy(
      EXPORTING
        iv_trkorr = iv_from
        iv_doc    = iv_doc
    ).
  CATCH zcx_trm_exception INTO lo_exc.
    PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.
