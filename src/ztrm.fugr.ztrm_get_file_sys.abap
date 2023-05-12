FUNCTION ZTRM_GET_FILE_SYS.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(EV_FILE_SYS) TYPE  FILESYS_D
*"  EXCEPTIONS
*"      NOT_FOUND
*"----------------------------------------------------------------------

  SELECT SINGLE filesys INTO ev_file_sys FROM opsystem WHERE opsys = sy-opsys.
  IF sy-subrc NE 0.
    RAISE not_found.
  ENDIF.
ENDFUNCTION.
