FUNCTION ZTRM_GET_BINARY_FILE.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_FILE_PATH) TYPE  STRING
*"  EXPORTING
*"     VALUE(EV_FILE) TYPE  XSTRING
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"----------------------------------------------------------------------
  CALL FUNCTION 'ZTRM_CHECK_AUTH'
    EXCEPTIONS
      trm_rfc_unauthorized = 1.
  IF sy-subrc EQ 1.
    RAISE trm_rfc_unauthorized.
  ENDIF.

  OPEN DATASET iv_file_path FOR INPUT IN BINARY MODE.
  READ DATASET iv_file_path INTO ev_file.
  CLOSE DATASET iv_file_path.


ENDFUNCTION.
