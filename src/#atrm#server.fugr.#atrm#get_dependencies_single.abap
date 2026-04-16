FUNCTION /atrm/get_dependencies_single.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(OBJECT) TYPE  /ATRM/OBJECT
*"  EXPORTING
*"     VALUE(DEPENDENCIES) TYPE  /ATRM/OBJECT_DEPENDENCY_T
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
      /atrm/cl_object_dispacher=>get(
        key = object
      )->get_dependencies(
        IMPORTING
          dependencies = dependencies[]
      ).
    CATCH /atrm/cx_exception INTO go_exc.
      PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.
