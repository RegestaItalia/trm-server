FUNCTION ztrm_write_binary_file.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_FILE_PATH) TYPE  STRING
*"     VALUE(IV_FILE) TYPE  XSTRING
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"----------------------------------------------------------------------
  PERFORM check_auth.

  OPEN DATASET iv_file_path FOR OUTPUT IN BINARY MODE.
  TRANSFER iv_file TO iv_file_path.
  CLOSE DATASET iv_file_path.



ENDFUNCTION.
