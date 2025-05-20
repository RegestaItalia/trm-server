FUNCTION ztrm_execute_post_activity.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_DATA) TYPE  XSTRING
*"     VALUE(IV_PRE) TYPE  FLAG OPTIONAL
*"  EXPORTING
*"     VALUE(EV_EXECUTE) TYPE  FLAG
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
      IF iv_pre EQ 'X'.
        zcl_trm_post_activity=>pre(
          EXPORTING
            iv_data     = iv_data
          IMPORTING
            et_messages = et_messages[]
            ev_execute  = ev_execute
        ).
      ELSE.
        zcl_trm_post_activity=>execute(
          EXPORTING
            iv_data     = iv_data
          IMPORTING
            et_messages = et_messages[]
        ).
      ENDIF.
    CATCH zcx_trm_exception INTO lo_exc.
      PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.
