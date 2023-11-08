FUNCTION ZTRM_SET_INTEGRITY.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IS_INTEGRITY) TYPE  ZTRM_INTEGRITY
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"----------------------------------------------------------------------
  CALL FUNCTION 'ZTRM_CHECK_AUTH'
    EXCEPTIONS
      trm_rfc_unauthorized = 1.
  IF sy-subrc EQ 1.
    RAISE trm_rfc_unauthorized.
  ENDIF.

  MODIFY ztrm_integrity FROM is_integrity.
  COMMIT WORK AND WAIT.



ENDFUNCTION.
