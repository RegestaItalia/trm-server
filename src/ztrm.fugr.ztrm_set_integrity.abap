FUNCTION ztrm_set_integrity.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IS_INTEGRITY) TYPE  ZTRM_INTEGRITY
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"----------------------------------------------------------------------
  PERFORM check_auth.

  MODIFY ztrm_integrity FROM is_integrity.
  COMMIT WORK AND WAIT.



ENDFUNCTION.
