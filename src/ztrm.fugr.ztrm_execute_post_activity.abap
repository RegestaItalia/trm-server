FUNCTION ztrm_execute_post_activity.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_DATA) TYPE  XSTRING
*"  TABLES
*"      ET_MESSAGES STRUCTURE  SYMSG
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      PA_NOT_FOUND
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
      zcl_trm_utility=>execute_post_activity(
        EXPORTING
          iv_data     = iv_data
        IMPORTING
          et_messages = et_messages[]
      ).
    CATCH zcx_trm_exception INTO lo_exc.
      PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.
