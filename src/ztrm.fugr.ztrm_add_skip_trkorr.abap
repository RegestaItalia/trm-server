FUNCTION ZTRM_ADD_SKIP_TRKORR.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_TRKORR) TYPE  TRKORR
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INSERT_ERROR
*"----------------------------------------------------------------------
  CALL FUNCTION 'ZTRM_CHECK_AUTH'
    EXCEPTIONS
      trm_rfc_unauthorized = 1.
  IF sy-subrc EQ 1.
    RAISE trm_rfc_unauthorized.
  ENDIF.

  DATA ls_data TYPE ztrm_skip_trkorr.
  ls_data-trkorr = iv_trkorr.

  INSERT ztrm_skip_trkorr FROM ls_data.
  COMMIT WORK AND WAIT.

  IF sy-subrc <> 0.
    RAISE insert_error.
  ENDIF.

ENDFUNCTION.
