FUNCTION ztrm_set_artifact_devc.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IS_MAPPING) TYPE  ZTRM_DEVC
*"  EXCEPTIONS
*"      MODIFY_ERROR
*"----------------------------------------------------------------------
  MODIFY ztrm_devc FROM is_mapping.
  COMMIT WORK AND WAIT.

  IF sy-subrc NE 0.
    RAISE modify_error.
  ENDIF.


ENDFUNCTION.
