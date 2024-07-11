FUNCTION ztrm_add_objs_tr.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_LOCK) TYPE  FLAG
*"     VALUE(IV_TRKORR) TYPE  TRKORR
*"  EXPORTING
*"     VALUE(ET_LOG) TYPE  SPROT_U_TAB
*"  TABLES
*"      IT_E071 STRUCTURE  E071
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_REQUEST
*"      INVALID_REQUEST_TYPE
*"      USER_NOT_OWNER
*"      NO_OBJECTS_APPENDED
*"      ENQUEUE_ERROR
*"      CANCELLED_BY_USER
*"      RECURSIVE_CALL
*"----------------------------------------------------------------------
  PERFORM check_auth.

  CALL FUNCTION 'TRINT_REQUEST_CHOICE'
    EXPORTING
      iv_suppress_dialog   = 'X'
      iv_request_types     = 'FTCOK'
      iv_lock_objects      = iv_lock
      iv_with_error_log    = 'X'
      iv_request           = iv_trkorr
    IMPORTING
      et_log               = et_log
    TABLES
      it_e071              = it_e071
    EXCEPTIONS
      invalid_request      = 1
      invalid_request_type = 2
      user_not_owner       = 3
      no_objects_appended  = 4
      enqueue_error        = 5
      cancelled_by_user    = 6
      recursive_call       = 7.

  IF sy-subrc EQ 1.
    RAISE invalid_request.
  ELSEIF sy-subrc EQ 2.
    RAISE invalid_request_type.
  ELSEIF sy-subrc EQ 3.
    RAISE user_not_owner.
  ELSEIF sy-subrc EQ 4.
    RAISE no_objects_appended.
  ELSEIF sy-subrc EQ 5.
    RAISE enqueue_error.
  ELSEIF sy-subrc EQ 6.
    RAISE cancelled_by_user.
  ELSEIF sy-subrc EQ 7.
    RAISE recursive_call.
  ENDIF.
ENDFUNCTION.
