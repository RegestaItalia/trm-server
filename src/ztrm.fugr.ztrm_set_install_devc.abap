FUNCTION ztrm_set_install_devc.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      IT_INSTALLDEVC STRUCTURE  ZTRM_INSTALLDEVC
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"----------------------------------------------------------------------
  PERFORM check_auth.

  DATA ls_installdevc LIKE LINE OF it_installdevc.
  LOOP AT it_installdevc INTO ls_installdevc.
    MODIFY ztrm_installdevc FROM ls_installdevc.
  ENDLOOP.

  COMMIT WORK AND WAIT.



ENDFUNCTION.
