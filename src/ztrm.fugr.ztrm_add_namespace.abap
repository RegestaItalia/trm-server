FUNCTION ztrm_add_namespace.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_NAMESPACE) TYPE  NAMESPACE
*"     VALUE(IV_REPLICENSE) TYPE  TRNLICENSE
*"  TABLES
*"      IT_TEXTS STRUCTURE  TRNSPACETT
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INSERT_ERROR
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.
  TRY.
      zcl_trm_utility=>add_namespace(
        EXPORTING
          iv_namespace  = iv_namespace
          iv_replicense = iv_replicense
          it_texts      = it_texts[]
      ).
    CATCH zcx_trm_exception INTO lo_exc.
      PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.
