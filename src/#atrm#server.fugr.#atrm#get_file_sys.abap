FUNCTION /ATRM/GET_FILE_SYS.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(FILE_SYS) TYPE  FILESYS
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      NOT_FOUND
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
    /ATRM/CL_UTILITIES=>get_file_sys(
      IMPORTING
        file_sys = file_sys
    ).
  CATCH /atrm/cx_exception INTO go_exc.
    PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.
