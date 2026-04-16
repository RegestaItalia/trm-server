FUNCTION /ATRM/GET_DEPENDENCIES.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(DEVCLASS) TYPE  DEVCLASS
*"     VALUE(INCL_SUB) TYPE  FLAG
*"     VALUE(LOG_ID) TYPE  /ATRM/POLLING_ID OPTIONAL
*"  EXPORTING
*"     VALUE(DEPENDENCIES) TYPE  /ATRM/OBJECT_DEPENDENCIES_T
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
      dependencies = /atrm/cl_object_dispacher=>get_package_dependencies(
        EXPORTING
          package      = devclass
          incl_sub     = incl_sub
          log_id       = log_id
      ).
    CATCH /atrm/cx_exception INTO go_exc.
      PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.
