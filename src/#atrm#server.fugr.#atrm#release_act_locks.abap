FUNCTION /atrm/release_act_locks.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(OWNER_TOKEN) TYPE  SYSUUID_C32
*"     VALUE(KEYS) TYPE  /ATRM/ACT_LOCK_T
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.
  TRY.
      CALL METHOD ('/ATRM/CL_ACTION_LOCK')=>('RELEASE')
        EXPORTING
          it_keys = keys
          iv_owner_token = owner_token.
    CATCH /atrm/cx_exception INTO go_exc.
      PERFORM handle_exception.
  ENDTRY.
ENDFUNCTION.
