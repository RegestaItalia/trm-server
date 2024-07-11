FUNCTION ztrm_delete_transport.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_TRKORR) TYPE  TRKORR
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      FILE_ACCESS_ERROR
*"      ORDER_ALREADY_RELEASED
*"      ORDER_CONTAINS_C_MEMBER
*"      ORDER_CONTAINS_LOCKED_ENTRIES
*"      ORDER_IS_REFERED
*"      REPAIR_ORDER
*"      USER_NOT_OWNER
*"      DELETE_WAS_CANCELLED
*"      ORDERNUMBER_EMPTY
*"      TR_ENQUEUE_FAILED
*"      OBJECTS_FREE_BUT_STILL_LOCKS
*"      ORDER_LOCK_FAILED
*"      NO_AUTHORIZATION
*"      WRONG_CLIENT
*"      PROJECT_STILL_REFERENCED
*"      SUCCESSORS_ALREADY_RELEASED
*"----------------------------------------------------------------------
  PERFORM check_auth.

  CALL FUNCTION 'TR_DELETE_COMM'
    EXPORTING
      wi_dialog                     = ' '
      wi_trkorr                     = iv_trkorr
    EXCEPTIONS
      file_access_error             = 1
      order_already_released        = 2
      order_contains_c_member       = 3
      order_contains_locked_entries = 4
      order_is_refered              = 5
      repair_order                  = 6
      user_not_owner                = 7
      delete_was_cancelled          = 8
      ordernumber_empty             = 9
      tr_enqueue_failed             = 10
      objects_free_but_still_locks  = 11
      order_lock_failed             = 12
      no_authorization              = 13
      wrong_client                  = 14
      project_still_referenced      = 15
      successors_already_released   = 16.

  IF sy-subrc EQ 1.
    RAISE file_access_error.
  ELSEIF sy-subrc EQ 2.
    RAISE order_already_released.
  ELSEIF sy-subrc EQ 3.
    RAISE order_contains_c_member.
  ELSEIF sy-subrc EQ 4.
    RAISE order_contains_locked_entries.
  ELSEIF sy-subrc EQ 5.
    RAISE order_is_refered.
  ELSEIF sy-subrc EQ 6.
    RAISE repair_order.
  ELSEIF sy-subrc EQ 7.
    RAISE user_not_owner.
  ELSEIF sy-subrc EQ 8.
    RAISE delete_was_cancelled.
  ELSEIF sy-subrc EQ 9.
    RAISE ordernumber_empty.
  ELSEIF sy-subrc EQ 10.
    RAISE tr_enqueue_failed.
  ELSEIF sy-subrc EQ 11.
    RAISE objects_free_but_still_locks.
  ELSEIF sy-subrc EQ 12.
    RAISE order_lock_failed.
  ELSEIF sy-subrc EQ 13.
    RAISE no_authorization.
  ELSEIF sy-subrc EQ 14.
    RAISE wrong_client.
  ELSEIF sy-subrc EQ 15.
    RAISE project_still_referenced.
  ELSEIF sy-subrc EQ 16.
    RAISE successors_already_released.
  ENDIF.


ENDFUNCTION.
