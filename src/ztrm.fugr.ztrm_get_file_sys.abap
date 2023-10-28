FUNCTION ZTRM_GET_FILE_SYS.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(EV_FILE_SYS) TYPE  FILESYS
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      NOT_FOUND
*"----------------------------------------------------------------------
  CALL FUNCTION 'ZTRM_CHECK_AUTH'
    EXCEPTIONS
      trm_rfc_unauthorized = 1.
  IF sy-subrc EQ 1.
    RAISE trm_rfc_unauthorized.
  ENDIF.


  SELECT SINGLE filesys INTO ev_file_sys FROM opsystem WHERE opsys = sy-opsys.
  IF sy-subrc NE 0.
    RAISE not_found.
  ENDIF.
ENDFUNCTION.
