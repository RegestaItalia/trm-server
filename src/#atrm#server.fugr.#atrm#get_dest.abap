FUNCTION /atrm/get_dest.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(DEST) TYPE  SYSYSID
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
      dest = /atrm/cl_utilities=>get_dest( ).
    CATCH /atrm/cx_exception INTO go_exc.
      PERFORM handle_exception.
  ENDTRY.


ENDFUNCTION.
