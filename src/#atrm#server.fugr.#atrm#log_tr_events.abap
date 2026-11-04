FUNCTION /ATRM/LOG_TR_EVENTS.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(ACTION_ID) TYPE  /ATRM/LOG_ACTION_ID
*"     VALUE(EVENTS) TYPE  /ATRM/LOG_EVT_IN_T
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      ENQUEUE_ERROR
*"      NOT_FOUND
*"      INSERT_ERROR
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.
  TRY.
      CALL METHOD ('/ATRM/CL_ACTION_LOG')=>('ADD_EVENTS')
        EXPORTING
          iv_action_id = action_id
          it_events = events.
    CATCH /atrm/cx_exception INTO go_exc.
      PERFORM handle_exception.
  ENDTRY.
ENDFUNCTION.
