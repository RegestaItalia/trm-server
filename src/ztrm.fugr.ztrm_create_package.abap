FUNCTION ztrm_create_package.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IS_DATA) TYPE  SCOMPKDTLN
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      DYN_CALL_PARAM_NOT_FOUND
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
      zcl_trm_package=>create(
        EXPORTING
          is_data = is_data
      ).
    CATCH zcx_trm_exception INTO lo_exc.
      PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.
