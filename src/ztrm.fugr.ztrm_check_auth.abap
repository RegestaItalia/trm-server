FUNCTION ztrm_check_auth.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"----------------------------------------------------------------------

  SELECT COUNT( * ) FROM ztrm_users WHERE uname = sy-uname.

  IF sy-dbcnt NE 1.
    RAISE trm_rfc_unauthorized.
  ENDIF.



ENDFUNCTION.
