FUNCTION /ATRM/GET_DEVCLASS_OBJS.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(DEVCLASS) TYPE  DEVCLASS
*"  EXPORTING
*"     VALUE(TADIR) TYPE  SCTS_TADIR
*"  EXCEPTIONS
*"      TRM_RFC_UNAUTHORIZED
*"      INVALID_INPUT
*"      GENERIC
*"----------------------------------------------------------------------
  PERFORM check_auth.

  TRY.
    CREATE OBJECT go_package EXPORTING devclass = devclass.
    go_package->get_objects(
      IMPORTING
        tadir    = tadir[]
    ).
  CATCH /atrm/cx_exception INTO go_exc.
    PERFORM handle_exception.
  ENDTRY.

ENDFUNCTION.
