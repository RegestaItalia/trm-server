FUNCTION ZTRM_GET_SUBPACKAGES.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_DEVCLASS) TYPE  DEVCLASS
*"  TABLES
*"      ET_SUBPACKAGES STRUCTURE  TDEVC
*"  EXCEPTIONS
*"      NO_PACKAGE_SPECIFIED
*"      PACKAGE_HAS_NO_TDEVC_RECORD
*"      PACKAGE_HAS_NO_TADIR_RECORD
*"      PACKAGE_DOES_NOT_EXIST
*"      INVALID_SUPERPACKAGE
*"      NO_OUTPUT_PARAMETER_REQUESTED
*"----------------------------------------------------------------------
  DATA lt_subpackages TYPE cl_pak_package_queries=>tt_subpackage_info.
  cl_pak_package_queries=>get_all_subpackages(
    EXPORTING
      im_package                    = iv_devclass
      im_also_local_packages        = ' '
    IMPORTING
      et_subpackages                = lt_subpackages
    EXCEPTIONS
      no_package_specified          = 1
      package_has_no_tdevc_record   = 2
      package_has_no_tadir_record   = 3
      package_does_not_exist        = 4
      invalid_superpackage          = 5
      no_output_parameter_requested = 6 ).

  IF sy-subrc EQ 0.
    DATA ls_subpackage LIKE LINE OF lt_subpackages.
    DATA ls_tdevc LIKE LINE OF et_subpackages.
    LOOP AT lt_subpackages INTO ls_subpackage.
      ls_tdevc-devclass = ls_subpackage-package.
      APPEND ls_tdevc TO et_subpackages.
      CLEAR ls_tdevc.
      CLEAR ls_subpackage.
    ENDLOOP.
  ELSEIF sy-subrc EQ 1.
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
    RAISE no_output_parameter_requested.
  ENDIF.

ENDFUNCTION.
