FUNCTION ztrm_add_skip_trkorr.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_TRKORR) TYPE  TRKORR
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INSERT_ERROR
*"----------------------------------------------------------------------
  PERFORM check_auth.

  DATA ls_data TYPE ztrm_skip_trkorr.
  ls_data-trkorr = iv_trkorr.

  INSERT ztrm_skip_trkorr FROM ls_data.
  COMMIT WORK AND WAIT.

  IF sy-subrc <> 0.
    RAISE insert_error.
  ENDIF.

ENDFUNCTION.
