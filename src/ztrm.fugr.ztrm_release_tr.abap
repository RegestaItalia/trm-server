FUNCTION ZTRM_RELEASE_TR.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_TRKORR) TYPE  TRKORR
*"     VALUE(IV_LOCK) TYPE  FLAG
*"  EXPORTING
*"     VALUE(ET_MESSAGES) TYPE  CTSGERRMSGS
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      CTS_INITIALIZATION_FAILURE
*"      ENQUEUE_FAILED
*"      NO_AUTHORIZATION
*"      INVALID_REQUEST
*"      REQUEST_ALREADY_RELEASED
*"      REPEAT_TOO_EARLY
*"      OBJECT_LOCK_ERROR
*"      OBJECT_CHECK_ERROR
*"      DOCU_MISSING
*"      DB_ACCESS_ERROR
*"      ACTION_ABORTED_BY_USER
*"      EXPORT_FAILED
*"      EXECUTE_OBJECTS_CHECK
*"      RELEASE_IN_BG_MODE
*"      RELEASE_IN_BG_MODE_W_OBJCHK
*"      ERROR_IN_EXPORT_METHODS
*"      OBJECT_LANG_ERROR
*"----------------------------------------------------------------------
  CALL FUNCTION 'ZTRM_CHECK_AUTH'
    EXCEPTIONS
      trm_rfc_unauthorized = 1.
  IF sy-subrc EQ 1.
    RAISE trm_rfc_unauthorized.
  ENDIF.

  DATA lv_without_lock TYPE flag.
  IF iv_lock EQ 'X'.
    lv_without_lock = ' '.
  ELSE.
    lv_without_lock = 'X'.
  ENDIF.

  CALL FUNCTION 'TRINT_RELEASE_REQUEST'
    EXPORTING
      iv_trkorr                   = iv_trkorr
      iv_dialog                   = ' '
      iv_success_message          = ' '
      iv_display_export_log       = ' '
      iv_without_objects_check    = 'X'
      iv_without_locking          = lv_without_lock
    EXCEPTIONS
      cts_initialization_failure  = 1
      enqueue_failed              = 2
      no_authorization            = 3
      invalid_request             = 4
      request_already_released    = 5
      repeat_too_early            = 6
      object_lock_error           = 7
      object_check_error          = 8
      docu_missing                = 9
      db_access_error             = 10
      action_aborted_by_user      = 11
      export_failed               = 12
      execute_objects_check       = 13
      release_in_bg_mode          = 14
      release_in_bg_mode_w_objchk = 15
      error_in_export_methods     = 16
      object_lang_error           = 17.

  IF sy-subrc EQ 1.
    RAISE cts_initialization_failure.
  ELSEIF sy-subrc EQ 2.
    RAISE enqueue_failed.
  ELSEIF sy-subrc EQ 3.
    RAISE no_authorization.
  ELSEIF sy-subrc EQ 4.
    RAISE invalid_request.
  ELSEIF sy-subrc EQ 5.
    RAISE request_already_released.
  ELSEIF sy-subrc EQ 6.
    RAISE repeat_too_early.
  ELSEIF sy-subrc EQ 7.
    RAISE object_lock_error.
  ELSEIF sy-subrc EQ 8.
    RAISE object_check_error.
  ELSEIF sy-subrc EQ 9.
    RAISE docu_missing.
  ELSEIF sy-subrc EQ 10.
    RAISE db_access_error.
  ELSEIF sy-subrc EQ 11.
    RAISE action_aborted_by_user.
  ELSEIF sy-subrc EQ 12.
    RAISE export_failed.
  ELSEIF sy-subrc EQ 13.
    RAISE execute_objects_check.
  ELSEIF sy-subrc EQ 14.
    RAISE release_in_bg_mode.
  ELSEIF sy-subrc EQ 15.
    RAISE release_in_bg_mode_w_objchk.
  ELSEIF sy-subrc EQ 16.
    RAISE error_in_export_methods.
  ELSEIF sy-subrc EQ 17.
    RAISE object_lang_error.
  ENDIF.


ENDFUNCTION.
