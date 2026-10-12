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
*"      NOT_FOUND
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
    CALL METHOD ('/ATRM/CL_UTILITIES')=>('GET_BINARY_FILE')
      EXPORTING
        file_path = file_path
      IMPORTING
        file      = file
    .
  CATCH /atrm/cx_exception INTO go_exc.
    PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.
