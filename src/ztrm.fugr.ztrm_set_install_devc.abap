FUNCTION ZTRM_SET_INSTALL_DEVC.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      IT_INSTALLDEVC STRUCTURE  ZTRM_INSTALLDEVC
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"----------------------------------------------------------------------
  CALL FUNCTION 'ZTRM_CHECK_AUTH'
    EXCEPTIONS
      trm_rfc_unauthorized = 1.
  IF sy-subrc EQ 1.
    RAISE trm_rfc_unauthorized.
  ENDIF.

  DATA ls_installdevc LIKE LINE OF it_installdevc.
  LOOP AT it_installdevc INTO ls_installdevc.
    MODIFY ztrm_installdevc FROM ls_installdevc.
  ENDLOOP.

  COMMIT WORK AND WAIT.



ENDFUNCTION.
