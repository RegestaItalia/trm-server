FUNCTION /atrm/get_dot_abapgit.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(DEVCLASS) TYPE  DEVCLASS
*"  EXPORTING
*"     VALUE(DOT_ABAPGIT) TYPE  XSTRING
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      ABAPGIT_INTERGRATION
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
      dot_abapgit = /atrm/cl_abapgit=>get_dot_abapgit(
        EXPORTING
          devclass    = devclass
      ).
    CATCH /atrm/cx_exception INTO go_exc.
      PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.
