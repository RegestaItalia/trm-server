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

  DATA lo_singleton TYPE REF TO /atrm/cl_singleton.

  TRY.
    lo_singleton = /atrm/cl_singleton=>get( ).
    CALL METHOD lo_singleton->get_supported_object_types
      IMPORTING
        object_text = object_text[].
  CATCH /atrm/cx_exception INTO go_exc.
    PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.
