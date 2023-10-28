FUNCTION ZTRM_CHECK_AUTH.
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
