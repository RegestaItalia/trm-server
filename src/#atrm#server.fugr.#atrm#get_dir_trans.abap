FUNCTION /atrm/get_dir_trans.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(DIR_TRANS) TYPE  PFEVALUE
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
    CALL METHOD ('/ATRM/CL_UTILITIES')=>('GET_DIR_TRANS')
      IMPORTING
        dir_trans = dir_trans
    .
  CATCH /atrm/cx_exception INTO go_exc.
    PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.
