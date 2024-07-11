FUNCTION ztrm_check_auth.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"----------------------------------------------------------------------
  IF zcl_trm_utility=>check_functions_authorization( ) NE 'X'.
    RAISE trm_rfc_unauthorized.
  ENDIF.

ENDFUNCTION.
