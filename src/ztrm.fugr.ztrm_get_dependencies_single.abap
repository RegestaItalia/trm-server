FUNCTION ztrm_get_dependencies_single.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IS_OBJECT) TYPE  ZTRM_OBJECT
*"  EXPORTING
*"     VALUE(ET_DEPENDENCIES) TYPE  ZTRM_OBJECT_DEPENDENCY_T
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
      zcl_trm_object_dispacher=>get(
        key = is_object
      )->get_dependencies(
        IMPORTING
          et_dependencies = et_dependencies
      ).
    CATCH zcx_trm_exception INTO lo_exc.
      PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.
