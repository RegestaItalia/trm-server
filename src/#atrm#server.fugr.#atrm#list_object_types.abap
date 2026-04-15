FUNCTION /atrm/list_object_types.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      OBJECT_TEXT STRUCTURE  KO100
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
    /atrm/cl_singleton=>get( )->get_supported_object_types(
      IMPORTING
        object_text = object_text[]
    ).
  CATCH /atrm/cx_exception INTO go_exc.
    PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.
