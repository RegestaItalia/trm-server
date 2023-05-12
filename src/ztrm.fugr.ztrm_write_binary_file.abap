FUNCTION ZTRM_WRITE_BINARY_FILE.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_FILE_PATH) TYPE  STRING
*"     VALUE(IV_FILE) TYPE  XSTRING
*"----------------------------------------------------------------------
  OPEN DATASET iv_file_path FOR OUTPUT IN BINARY MODE.
  TRANSFER iv_file TO iv_file_path.
  CLOSE DATASET iv_file_path.



ENDFUNCTION.
