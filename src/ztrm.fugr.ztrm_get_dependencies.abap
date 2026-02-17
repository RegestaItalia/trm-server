FUNCTION ztrm_get_dependencies.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_DEVCLASS) TYPE  DEVCLASS
*"     VALUE(IV_INCL_SUB) TYPE  FLAG
*"     VALUE(IV_LOG_ID) TYPE  ZTRM_POLLING_ID OPTIONAL
*"  EXPORTING
*"     VALUE(ET_DEPENDENCIES) TYPE  ZTRM_OBJECT_DEPENDENCIES_T
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
      et_dependencies = zcl_trm_object_dispacher=>get_package_dependencies(
        EXPORTING
          package      = iv_devclass
          incl_sub     = iv_incl_sub
          log_id       = iv_log_id
      ).
    CATCH zcx_trm_exception INTO lo_exc.
      PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.
