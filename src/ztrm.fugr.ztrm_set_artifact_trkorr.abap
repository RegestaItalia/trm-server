FUNCTION ztrm_set_artifact_trkorr.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IS_DATA) TYPE  ZTRM_TRKORR
*"  EXCEPTIONS
*"      MODIFY_ERROR
*"----------------------------------------------------------------------
  MODIFY ztrm_trkorr FROM is_data.
  COMMIT WORK AND WAIT.

  IF sy-subrc NE 0.
    RAISE modify_error.
  ENDIF.


ENDFUNCTION.
