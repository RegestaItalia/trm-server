FUNCTION /ATRM/GET_BINARY_FILE.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(FILE_PATH) TYPE  STRING
*"  EXPORTING
*"     VALUE(FILE) TYPE  XSTRING
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
    /ATRM/CL_UTILITIES=>get_binary_file(
      EXPORTING
        file_path = file_path
      IMPORTING
        file      = file
    ).
  CATCH /atrm/cx_exception INTO go_exc.
    PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.
