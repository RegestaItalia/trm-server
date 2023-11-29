FUNCTION ZTRM_TDEVC_INTERFACE.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_DEVCLASS) TYPE  DEVCLASS
*"     VALUE(IV_PARENTCL) TYPE  DEVCLASS OPTIONAL
*"     VALUE(IV_RM_PARENTCL) TYPE  FLAG OPTIONAL
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      FACTORY_ERROR
*"----------------------------------------------------------------------
  CALL FUNCTION 'ZTRM_CHECK_AUTH'
    EXCEPTIONS
      trm_rfc_unauthorized = 1.
  IF sy-subrc EQ 1.
    RAISE trm_rfc_unauthorized.
  ENDIF.

  IF iv_rm_parentcl EQ 'X'.
    "SAP Note 636704
    DATA: ls_modify_sign TYPE scompksign,
          ls_pack_data   TYPE scompkdtln,
          ls_cr          TYPE e070-trkorr.
    ls_modify_sign-parentcl = 'X'.
    ls_pack_data-devclass   = iv_devclass.
*    CALL FUNCTION 'PA_MAINTAIN_PACKAGE_DARK'
*      EXPORTING
*        i_operation        = 'MODIFY'
*        i_modify_data_sign = ls_modify_sign
*        i_suppress_dialog  = 'X'
*      CHANGING
*        c_package_data     = ls_pack_data
*      EXCEPTIONS
*        OTHERS             = 1.
*    IF sy-subrc <> 0.
*      RAISE factory_error.
*    ENDIF.
    PERFORM modify_package_data
            USING    ls_modify_sign
                     'X'
            CHANGING ls_cr
                     ls_pack_data.
  ELSEIF iv_parentcl IS NOT INITIAL.
    DATA lo_package TYPE REF TO if_package.
    cl_package_factory=>load_package(
      EXPORTING
        i_package_name        = iv_devclass
      IMPORTING
        e_package             = lo_package
      EXCEPTIONS
        OTHERS                = 1 ).
    lo_package->set_changeable(
    EXPORTING
      i_changeable              = 'X'
      i_suppress_dialog         = 'D'
    EXCEPTIONS
      object_already_changeable = 1
      object_already_unlocked   = 2
      OTHERS                    = 3 ).
    IF sy-subrc EQ 3.
      RAISE factory_error.
    ENDIF.
    lo_package->set_super_package_name(
      EXPORTING
        i_super_package_name = iv_parentcl
      EXCEPTIONS
        OTHERS               = 1 ).
    IF sy-subrc <> 0.
      RAISE factory_error.
    ENDIF.
    lo_package->save(
      EXPORTING
        i_suppress_dialog      = 'X'
        i_suppress_corr_insert = 'X'
      EXCEPTIONS
        OTHERS                = 1 ).
    IF sy-subrc <> 0.
      RAISE factory_error.
    ENDIF.
    lo_package->set_changeable(
      EXPORTING
        i_changeable              = 'X'
        i_suppress_dialog         = 'D'
      EXCEPTIONS
        object_already_changeable = 1
        object_already_unlocked   = 2
        OTHERS                    = 3 ).
    IF sy-subrc EQ 3.
      RAISE factory_error.
    ENDIF.
  ENDIF.




ENDFUNCTION.

FORM modify_package_data
     USING    p_package_data_sign TYPE scompksign
              p_suppress_dialog   TYPE flag
     CHANGING p_transport_request TYPE e070-trkorr
              p_package_data      TYPE scompkdtln.

  DATA: subrc LIKE sy-subrc.
  DATA: lo_package TYPE REF TO if_package.

* load package
  CALL METHOD cl_package_factory=>load_package
    EXPORTING
      i_package_name             = p_package_data-devclass
      i_force_reload             = 'X'
    IMPORTING
      e_package                  = lo_package
    EXCEPTIONS
      object_not_existing        = 1
      unexpected_error           = 2
      intern_err                 = 3
      no_access                  = 4
      object_locked_and_modified = 5.
*
  IF sy-subrc <> 0.
    CASE sy-subrc.
      WHEN 1.
        RAISE package_not_existing.
      WHEN 4.
        RAISE no_authorization.
      WHEN 5.
        RAISE locked_by_other_user.
      WHEN OTHERS.
        RAISE internal_error.
    ENDCASE.
  ENDIF.

* lock package
  CALL METHOD lo_package->set_changeable
    EXPORTING
      i_changeable                = 'X'
      i_suppress_dialog           = 'D'
    EXCEPTIONS
      object_locked_by_other_user = 1
      permission_failure          = 2
      object_already_changeable   = 0                       "ignore it
      object_already_unlocked     = 4
      object_just_created         = 5
      object_deleted              = 6
      object_modified             = 7
      object_not_existing         = 8
      object_invalid              = 9
      unexpected_error            = 10.
*
  IF sy-subrc <> 0.
    CASE sy-subrc.
      WHEN 1.
        RAISE locked_by_other_user.
      WHEN 2.
        RAISE no_authorization.
      WHEN OTHERS.
        RAISE internal_error.
    ENDCASE.
  ENDIF.

* modify package
  CALL METHOD lo_package->set_all_attributes
    EXPORTING
      i_package_data             = p_package_data
      i_data_sign                = p_package_data_sign
    EXCEPTIONS
      object_not_changeable      = 1
      object_deleted             = 2
      object_invalid             = 3
      short_text_missing         = 4
      author_not_existing        = 5
      local_package              = 6
      software_component_invalid = 7
      layer_invalid              = 8
      korrflag_invalid           = 9
      component_not_existing     = 10
      component_missing          = 11
      authorize_failure          = 12
      prefix_in_use              = 13
      unexpected_error           = 14
      intern_err                 = 15
      wrong_mainpack_value       = 16
      superpackage_invalid       = 17.
*
  IF sy-subrc <> 0.
*   try to unlock the package, exceptions are tolerated
    subrc = sy-subrc.

    CALL METHOD lo_package->set_changeable
      EXPORTING
        i_changeable = ' '
      EXCEPTIONS
        OTHERS       = 0.

    CASE subrc.
      WHEN 4.
        RAISE short_text_missing.
      WHEN 5.
        RAISE invalid_author.
      WHEN 6.
        RAISE invalid_transport_properties.
      WHEN 7.
        RAISE invalid_software_component.
      WHEN 8.
        RAISE invalid_transport_layer.
      WHEN 9.
        RAISE invalid_transport_properties.
      WHEN 10.
        RAISE invalid_application_component.
      WHEN 11.
        RAISE invalid_application_component.
      WHEN 12.
        RAISE no_authorization.
      WHEN 16.
        RAISE wrong_mainpack_value.
      WHEN 17.
        RAISE superpackage_invalid.
      WHEN OTHERS.
        RAISE internal_error.
    ENDCASE.
  ENDIF. " IF sy-subrc <> 0.

* save package
  CALL METHOD lo_package->save
    EXPORTING
      i_transport_request    = p_transport_request
      i_suppress_dialog      = p_suppress_dialog
      i_suppress_corr_insert = 'X'
    IMPORTING
      e_transport_request    = p_transport_request
    EXCEPTIONS
      object_invalid         = 1
      object_not_changeable  = 2
      cancelled_in_corr      = 3
      permission_failure     = 4
      unexpected_error       = 5
      intern_err             = 6.
*
  IF sy-subrc <> 0.
*   try to undo the changes, exceptions are tolerated
*   (Note: if successful, this also unlocks the package)
    subrc = sy-subrc.
    CALL METHOD lo_package->undo_all_changes
      EXCEPTIONS
        OTHERS = 0.
    CASE subrc.
      WHEN 3.
        RAISE cancelled_in_corr.
      WHEN 4.
        RAISE no_authorization.
      WHEN OTHERS.
        RAISE internal_error.
    ENDCASE.
  ENDIF.

* unlock package
  CALL METHOD lo_package->set_changeable
    EXPORTING
      i_changeable                = ' '
      i_suppress_dialog           = 'D'
    EXCEPTIONS
      object_locked_by_other_user = 1
      permission_failure          = 2
      object_already_changeable   = 3
      object_already_unlocked     = 0                       "ignore
      object_just_created         = 5
      object_deleted              = 6
      object_modified             = 7
      object_not_existing         = 8
      object_invalid              = 9
      unexpected_error            = 10.
*
  IF sy-subrc <> 0.
    RAISE internal_error.
  ENDIF.
*
ENDFORM.
