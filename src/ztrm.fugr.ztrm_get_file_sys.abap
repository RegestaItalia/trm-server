FUNCTION ztrm_get_file_sys.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(EV_FILE_SYS) TYPE  FILESYS
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      NOT_FOUND
*"----------------------------------------------------------------------
  PERFORM check_auth.


  SELECT SINGLE filesys INTO ev_file_sys FROM opsystem WHERE opsys = sy-opsys.
  IF sy-subrc NE 0.
    RAISE not_found.
  ENDIF.
ENDFUNCTION.
