FUNCTION ZTRM_CREATE_PACKAGE.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IS_DATA) TYPE  SCOMPKDTLN
*"     VALUE(IV_TRKORR) TYPE  TRKORR OPTIONAL
*"  EXCEPTIONS
*"      EXCEPTION_RAISED
*"----------------------------------------------------------------------
  DATA lo_package TYPE REF TO if_package.
  cl_package_factory=>create_new_package(
    EXPORTING
      i_reuse_deleted_object     = abap_true
      i_suppress_dialog          = abap_true " does not exist in 730
    IMPORTING
      e_package                  = lo_package
    CHANGING
      c_package_data             = is_data
    EXCEPTIONS
      object_already_existing = 1
      object_just_created = 2
      not_authorized = 3
      wrong_name_prefix = 4
      undefined_name = 5
      reserved_local_name = 6
      invalid_package_name = 7
      short_text_missing = 8
      software_component_invalid = 9
      layer_invalid = 10
      author_not_existing = 11
      component_not_existing = 12
      component_missing = 13
      prefix_in_use = 14
      unexpected_error = 15
      intern_err = 16
      no_access = 17
      invalid_translation_depth = 18
      wrong_mainpack_value = 19
      superpackage_invalid = 20
      error_in_cts_checks = 21 ).

  IF sy-subrc NE 0.
    RAISE exception_raised.
  ENDIF.

  lo_package->save(
    EXPORTING
      i_suppress_dialog     = abap_true    " Controls whether popups can be transmitted
      i_transport_request   = iv_trkorr
      i_suppress_corr_insert = 'X'
    EXCEPTIONS
      OTHERS                = 1 ).

  IF sy-subrc NE 0.
    RAISE exception_raised.
  ENDIF.

  lo_package->set_changeable(
    EXPORTING
      i_changeable      = abap_false
      i_suppress_dialog = abap_true
    EXCEPTIONS
      OTHERS                = 1 ).

  IF sy-subrc NE 0.
    RAISE exception_raised.
  ENDIF.

  COMMIT WORK AND WAIT.

ENDFUNCTION.
