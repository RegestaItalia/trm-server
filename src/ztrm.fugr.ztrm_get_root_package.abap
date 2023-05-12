FUNCTION ZTRM_GET_ROOT_PACKAGE.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_PACKAGE) TYPE  DEVCLASS
*"  EXPORTING
*"     VALUE(EV_ROOT_PACKAGE) TYPE  DEVCLASS
*"  EXCEPTIONS
*"      NO_PACKAGE_SPECIFIED
*"      PACKAGE_HAS_NO_TDEVC_RECORD
*"      PACKAGE_HAS_NO_TADIR_RECORD
*"      PACKAGE_DOES_NOT_EXIST
*"      INVALID_SUPERPACKAGE
*"      PACKAGE_HIERARCHY_ERROR
*"      NO_OUTPUT_PARAMETER_REQUESTED
*"----------------------------------------------------------------------
  cl_pak_package_queries=>get_root_package(
    EXPORTING
      im_package                    = iv_package
    IMPORTING
      ev_root_package               = ev_root_package
    EXCEPTIONS
      no_package_specified          = 1
      package_has_no_tdevc_record   = 2
      package_has_no_tadir_record   = 3
      package_does_not_exist        = 4
      invalid_superpackage          = 5
      package_hierarchy_error       = 6
      no_output_parameter_requested = 7
  ).
  IF sy-subrc EQ 1.
    RAISE no_package_specified.
  ELSEIF sy-subrc EQ 2.
    RAISE package_has_no_tdevc_record.
  ELSEIF sy-subrc EQ 3.
    RAISE package_has_no_tadir_record.
  ELSEIF sy-subrc EQ 4.
    RAISE package_does_not_exist.
  ELSEIF sy-subrc EQ 5.
    RAISE invalid_superpackage.
  ELSEIF sy-subrc EQ 6.
    RAISE package_hierarchy_error.
  ELSEIF sy-subrc EQ 7.
    RAISE no_output_parameter_requested.
  ENDIF.




ENDFUNCTION.
