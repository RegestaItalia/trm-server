FUNCTION ztrm_regen_prog.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_PROGNAME) TYPE  PROGNAME
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      PROGRAM_NOT_FOUND
*"      GENERIC
*"----------------------------------------------------------------------

  PERFORM check_auth.

  TRY.
      zcl_trm_utility=>regen_program( iv_progname = iv_progname ).
    CATCH zcx_trm_exception INTO lo_exc.
      PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.
