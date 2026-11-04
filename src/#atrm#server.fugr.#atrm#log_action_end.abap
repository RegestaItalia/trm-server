FUNCTION /ATRM/LOG_ACTION_END.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(ACTION_ID) TYPE  /ATRM/LOG_ACTION_ID
*"     VALUE(STATUS) TYPE  /ATRM/LOG_STATUS
*"     VALUE(VERSION) TYPE  /ATRM/LOG_VERSION OPTIONAL
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      ENQUEUE_ERROR
*"      NOT_FOUND
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.
  TRY.
      CALL METHOD ('/ATRM/CL_ACTION_LOG')=>('END_ACTION')
        EXPORTING
          iv_action_id = action_id
          iv_status = status
          iv_version = version.
    CATCH /atrm/cx_exception INTO go_exc.
      PERFORM handle_exception.
  ENDTRY.
ENDFUNCTION.
