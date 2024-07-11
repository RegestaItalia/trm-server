FUNCTION ztrm_get_binary_file.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_FILE_PATH) TYPE  STRING
*"  EXPORTING
*"     VALUE(EV_FILE) TYPE  XSTRING
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"----------------------------------------------------------------------
  PERFORM check_auth.

  OPEN DATASET iv_file_path FOR INPUT IN BINARY MODE.
  READ DATASET iv_file_path INTO ev_file.
  CLOSE DATASET iv_file_path.


ENDFUNCTION.
