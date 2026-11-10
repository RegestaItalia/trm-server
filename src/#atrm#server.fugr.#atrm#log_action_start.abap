FUNCTION /ATRM/LOG_ACTION_START.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(ACTION_ID) TYPE  /ATRM/LOG_ACTION_ID
*"     VALUE(SESSION_ID) TYPE  /ATRM/LOG_SESSION_ID
*"     VALUE(PARENT_ACTION_ID) TYPE  /ATRM/LOG_ACTION_ID OPTIONAL
*"     VALUE(ACTION_NAME) TYPE  /ATRM/LOG_ACTION_NAME
*"     VALUE(PACKAGE_NAME) TYPE  /ATRM/PACKAGE_NAME OPTIONAL
*"     VALUE(PACKAGE_REGISTRY) TYPE  /ATRM/PACKAGE_REGISTRY OPTIONAL
*"     VALUE(VERSION) TYPE  /ATRM/LOG_VERSION OPTIONAL
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      ENQUEUE_ERROR
*"      INSERT_ERROR
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.
  TRY.
      CALL METHOD ('/ATRM/CL_ACTION_LOG')=>('START_ACTION')
        EXPORTING
          iv_action_id = action_id
          iv_session_id = session_id
          iv_parent_action_id = parent_action_id
          iv_action_name = action_name
          iv_package_name = package_name
          iv_package_registry = package_registry
          iv_version = version.
    CATCH /atrm/cx_exception INTO go_exc.
      PERFORM handle_exception.
  ENDTRY.
ENDFUNCTION.
