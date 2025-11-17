FUNCTION ztrm_list_object_types.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      ET_OBJECT_TEXT STRUCTURE  KO100
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
    zcl_trm_singleton=>get( )->get_supported_object_types(
      IMPORTING
        et_object_text = et_object_text[]
    ).
  CATCH zcx_trm_exception INTO lo_exc.
    PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.
