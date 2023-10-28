FUNCTION ZTRM_WRITE_BINARY_FILE.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_FILE_PATH) TYPE  STRING
*"     VALUE(IV_FILE) TYPE  XSTRING
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"----------------------------------------------------------------------
  CALL FUNCTION 'ZTRM_CHECK_AUTH'
    EXCEPTIONS
      trm_rfc_unauthorized = 1.
  IF sy-subrc EQ 1.
    RAISE trm_rfc_unauthorized.
  ENDIF.

  OPEN DATASET iv_file_path FOR OUTPUT IN BINARY MODE.
  TRANSFER iv_file TO iv_file_path.
  CLOSE DATASET iv_file_path.



ENDFUNCTION.
