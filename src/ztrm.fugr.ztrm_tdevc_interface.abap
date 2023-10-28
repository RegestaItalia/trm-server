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
          ls_pack_data   TYPE scompkdtln.
    ls_modify_sign-parentcl = 'X'.
    ls_pack_data-devclass   = iv_devclass.
    CALL FUNCTION 'PA_MAINTAIN_PACKAGE_DARK'
      EXPORTING
        i_operation        = 'MODIFY'
        i_modify_data_sign = ls_modify_sign
      CHANGING
        c_package_data     = ls_pack_data
      EXCEPTIONS
        OTHERS             = 1.
    IF sy-subrc <> 0.
      RAISE factory_error.
    ENDIF.
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
      i_changeable              = abap_true
      i_suppress_dialog         = abap_true
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
        i_suppress_dialog     = abap_true
      EXCEPTIONS
        OTHERS                = 1 ).
    IF sy-subrc <> 0.
      RAISE factory_error.
    ENDIF.
    lo_package->set_changeable(
      EXPORTING
        i_changeable              = abap_false
        i_suppress_dialog         = abap_true
      EXCEPTIONS
        object_already_changeable = 1
        object_already_unlocked   = 2
        OTHERS                    = 3 ).
    IF sy-subrc EQ 3.
      RAISE factory_error.
    ENDIF.
  ENDIF.




ENDFUNCTION.
