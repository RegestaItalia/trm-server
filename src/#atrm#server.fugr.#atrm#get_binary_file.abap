FUNCTION /atrm/get_binary_file.
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
    /atrm/cl_utilities=>get_binary_file(
      EXPORTING
        file_path = file_path
      IMPORTING
        file      = file
    ).
  CATCH /atrm/cx_exception INTO go_exc.
    PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.
