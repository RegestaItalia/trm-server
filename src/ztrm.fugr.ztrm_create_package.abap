FUNCTION ZTRM_CREATE_PACKAGE.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IS_DATA) TYPE  SCOMPKDTLN
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      OBJECT_ALREADY_EXISTING
*"      OBJECT_JUST_CREATED
*"      NOT_AUTHORIZED
*"      WRONG_NAME_PREFIX
*"      UNDEFINED_NAME
*"      RESERVED_LOCAL_NAME
*"      INVALID_PACKAGE_NAME
*"      SHORT_TEXT_MISSING
*"      SOFTWARE_COMPONENT_INVALID
*"      LAYER_INVALID
*"      AUTHOR_NOT_EXISTING
*"      COMPONENT_NOT_EXISTING
*"      COMPONENT_MISSING
*"      PREFIX_IN_USE
*"      UNEXPECTED_ERROR
*"      INTERN_ERR
*"      NO_ACCESS
*"      INVALID_TRANSLATION_DEPTH
*"      WRONG_MAINPACK_VALUE
*"      SUPERPACKAGE_INVALID
*"      ERROR_IN_CTS_CHECKS
*"      OBJECT_INVALID
*"      OBJECT_NOT_CHANGEABLE
*"      CANCELLED_IN_CORR
*"      PERMISSION_FAILURE
*"      OBJECT_LOCKED_BY_OTHER_USER
*"      OBJECT_ALREADY_CHANGEABLE
*"      OBJECT_ALREADY_UNLOCKED
*"      OBJECT_DELETED
*"      OBJECT_MODIFIED
*"      OBJECT_NOT_EXISTING
*"----------------------------------------------------------------------

  CALL FUNCTION 'ZTRM_CHECK_AUTH'
    EXCEPTIONS
      trm_rfc_unauthorized = 1.
  IF sy-subrc EQ 1.
    RAISE trm_rfc_unauthorized.
  ENDIF.


  DATA lo_package TYPE REF TO if_package.
  cl_package_factory=>create_new_package(
    EXPORTING
      i_reuse_deleted_object     = 'X'
      i_suppress_dialog          = 'X' " does not exist in 730
    IMPORTING
      e_package                  = lo_package
    CHANGING
      c_package_data             = is_data
    EXCEPTIONS
      object_already_existing    = 1
      object_just_created        = 2
      not_authorized             = 3
      wrong_name_prefix          = 4
      undefined_name             = 5
      reserved_local_name        = 6
      invalid_package_name       = 7
      short_text_missing         = 8
      software_component_invalid = 9
      layer_invalid              = 10
      author_not_existing        = 11
      component_not_existing     = 12
      component_missing          = 13
      prefix_in_use              = 14
      unexpected_error           = 15
      intern_err                 = 16
      no_access                  = 17
      invalid_translation_depth  = 18
      wrong_mainpack_value       = 19
      superpackage_invalid       = 20
      error_in_cts_checks        = 21 ).

  IF sy-subrc EQ 1.
    RAISE object_already_existing.
  ELSEIF sy-subrc EQ 2.
    RAISE object_just_created.
  ELSEIF sy-subrc EQ 3.
    RAISE not_authorized.
  ELSEIF sy-subrc EQ 4.
    RAISE wrong_name_prefix.
  ELSEIF sy-subrc EQ 5.
    RAISE undefined_name.
  ELSEIF sy-subrc EQ 6.
    RAISE reserved_local_name.
  ELSEIF sy-subrc EQ 7.
    RAISE invalid_package_name.
  ELSEIF sy-subrc EQ 8.
    RAISE short_text_missing.
  ELSEIF sy-subrc EQ 9.
    RAISE software_component_invalid.
  ELSEIF sy-subrc EQ 10.
    RAISE layer_invalid.
  ELSEIF sy-subrc EQ 11.
    RAISE author_not_existing.
  ELSEIF sy-subrc EQ 12.
    RAISE component_not_existing.
  ELSEIF sy-subrc EQ 13.
    RAISE component_missing.
  ELSEIF sy-subrc EQ 14.
    RAISE prefix_in_use.
  ELSEIF sy-subrc EQ 15.
    RAISE unexpected_error.
  ELSEIF sy-subrc EQ 16.
    RAISE intern_err.
  ELSEIF sy-subrc EQ 17.
    RAISE no_access.
  ELSEIF sy-subrc EQ 18.
    RAISE invalid_translation_depth.
  ELSEIF sy-subrc EQ 19.
    RAISE wrong_mainpack_value.
  ELSEIF sy-subrc EQ 20.
    RAISE superpackage_invalid.
  ELSEIF sy-subrc EQ 21.
    RAISE error_in_cts_checks.
  ENDIF.

  lo_package->save(
    EXPORTING
      i_suppress_dialog       = 'X'
      i_suppress_corr_insert  = 'X'
    EXCEPTIONS
      object_invalid          = 1
      object_not_changeable   = 2
      cancelled_in_corr       = 3
      permission_failure      = 4
      unexpected_error        = 5
      intern_err              = 6 ).

  IF sy-subrc EQ 1.
    RAISE object_invalid.
  ELSEIF sy-subrc EQ 2.
    RAISE object_not_changeable.
  ELSEIF sy-subrc EQ 3.
    RAISE cancelled_in_corr.
  ELSEIF sy-subrc EQ 4.
    RAISE permission_failure.
  ELSEIF sy-subrc EQ 5.
    RAISE unexpected_error.
  ELSEIF sy-subrc EQ 6.
    RAISE intern_err.
  ENDIF.

  lo_package->set_changeable(
    EXPORTING
      i_changeable                = ' '
      i_suppress_dialog           = 'X'
    EXCEPTIONS
      object_locked_by_other_user = 1
      permission_failure          = 2
      object_already_changeable   = 3
      object_already_unlocked     = 4
      object_just_created         = 5
      object_deleted              = 6
      object_modified             = 7
      object_not_existing         = 8
      object_invalid              = 9
      unexpected_error            = 10 ).

  IF sy-subrc EQ 1.
    RAISE object_locked_by_other_user.
  ELSEIF sy-subrc EQ 2.
    RAISE permission_failure.
  ELSEIF sy-subrc EQ 3.
    RAISE object_already_changeable.
  ELSEIF sy-subrc EQ 4.
    RAISE object_already_unlocked.
  ELSEIF sy-subrc EQ 5.
    RAISE object_just_created.
  ELSEIF sy-subrc EQ 6.
    RAISE object_deleted.
  ELSEIF sy-subrc EQ 7.
    RAISE object_modified.
  ELSEIF sy-subrc EQ 8.
    RAISE object_not_existing.
  ELSEIF sy-subrc EQ 9.
    RAISE object_invalid.
  ELSEIF sy-subrc EQ 10.
    RAISE unexpected_error.
  ENDIF.

ENDFUNCTION.
