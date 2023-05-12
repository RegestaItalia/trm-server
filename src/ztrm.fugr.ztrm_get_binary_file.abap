FUNCTION ZTRM_GET_BINARY_FILE.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_FILE_PATH) TYPE  STRING
*"  EXPORTING
*"     VALUE(EV_FILE) TYPE  XSTRING
*"----------------------------------------------------------------------
  OPEN DATASET iv_file_path FOR INPUT IN BINARY MODE.
  READ DATASET iv_file_path INTO ev_file.
  CLOSE DATASET iv_file_path.


ENDFUNCTION.
