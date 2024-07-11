FUNCTION ztrm_tr_copy.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_FROM) TYPE  TRKORR
*"     VALUE(IV_TO) TYPE  TRKORR
*"     VALUE(IV_DOC) TYPE  TRPARFLAG
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      DB_ACCESS_ERROR
*"      TRKORR_FROM_NOT_EXIST
*"      TRKORR_TO_IS_REPAIR
*"      TRKORR_TO_LOCKED
*"      TRKORR_TO_NOT_EXIST
*"      TRKORR_TO_RELEASED
*"      USER_NOT_OWNER
*"      NO_AUTHORIZATION
*"      WRONG_CLIENT
*"      WRONG_CATEGORY
*"      OBJECT_NOT_PATCHABLE
*"----------------------------------------------------------------------
  PERFORM check_auth.

  CALL FUNCTION 'TR_COPY_COMM'
    EXPORTING
      wi_dialog                = ' '
      wi_trkorr_from           = iv_from
      wi_trkorr_to             = iv_to
      wi_without_documentation = iv_doc
    EXCEPTIONS
      db_access_error          = 1
      trkorr_from_not_exist    = 2
      trkorr_to_is_repair      = 3
      trkorr_to_locked         = 4
      trkorr_to_not_exist      = 5
      trkorr_to_released       = 6
      user_not_owner           = 7
      no_authorization         = 8
      wrong_client             = 9
      wrong_category           = 10
      object_not_patchable     = 11.

  IF sy-subrc EQ 1.
    RAISE db_access_error.
  ELSEIF sy-subrc EQ 2.
    RAISE trkorr_from_not_exist.
  ELSEIF sy-subrc EQ 3.
    RAISE trkorr_to_is_repair.
  ELSEIF sy-subrc EQ 4.
    RAISE trkorr_to_locked.
  ELSEIF sy-subrc EQ 5.
    RAISE trkorr_to_not_exist.
  ELSEIF sy-subrc EQ 6.
    RAISE trkorr_to_released.
  ELSEIF sy-subrc EQ 7.
    RAISE user_not_owner.
  ELSEIF sy-subrc EQ 8.
    RAISE no_authorization.
  ELSEIF sy-subrc EQ 9.
    RAISE wrong_client.
  ELSEIF sy-subrc EQ 10.
    RAISE wrong_category.
  ELSEIF sy-subrc EQ 11.
    RAISE object_not_patchable.
  ENDIF.



ENDFUNCTION.
