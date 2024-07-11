FUNCTION ztrm_add_src_trkorr.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_TRKORR) TYPE  TRKORR
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INSERT_ERROR
*"----------------------------------------------------------------------
  PERFORM check_auth.

  DATA ls_data TYPE ztrm_src_trkorr.
  ls_data-trkorr = iv_trkorr.

  INSERT ztrm_src_trkorr FROM ls_data.
  COMMIT WORK AND WAIT.

  IF sy-subrc <> 0.
    RAISE insert_error.
  ENDIF.





ENDFUNCTION.
